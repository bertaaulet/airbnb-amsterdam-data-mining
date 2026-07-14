# ==============================================================================
# FEATURE ENGINEERING (EXTRACCIÓ, TRANSFORMACIÓ I SELECCIÓ)
# ==============================================================================

# ------------------------------------------------------------------------------
# BLOC 0: LLIBRERIES I CÀRREGA DE DADES
# ------------------------------------------------------------------------------
library(vcd)
library(rcompanion)
library(caret)
library(tidyverse)
library(mlbench)
library(corrplot)
library(XICOR)
library(randomForest)
library(inspectdf)
library(stringr)

# Carreguem el dataset que ve de l'script d'outliers
dd <- readRDS("../data/dataset_net_outliers.rds")

names(dd)
str(dd)

# Visió general inicial dels tipus de dades
inspect_types(dd) %>% show_plot()


# ------------------------------------------------------------------------------
# BLOC 1: COERCIÓ SELECTIVA DE TIPUS (Feature Transformation Base)
# ------------------------------------------------------------------------------

# 1.1. Passem IDs a caràcter per evitar que el model els calculi com a números
dd <- dd %>%
  mutate(across(c(id, host_id), as.character))

# 1.2. Coerció selectiva a FACTORS
vars_a_factor <- c("neighbourhood_cleansed", "property_type", "room_type", "source")
dd <- dd %>%
  mutate(across(all_of(vars_a_factor), as.factor))

# 1.3. Assegurem el format Date per a totes les columnes temporals
dd <- dd %>%
  mutate(across(c(first_review, last_review, host_since), as.Date))


# ------------------------------------------------------------------------------
# BLOC 2: AUDITORIES EXPLORATÒRIES
# ------------------------------------------------------------------------------

# 2.1. AUDITORIA DE PARETO (Concentració Categòrica)
vars_cat <- names(dd)[sapply(dd, function(x) is.factor(x) | is.character(x))]
auditoria_pareto <- sapply(vars_cat, function(v) {
  tab <- table(dd[[v]], useNA = "no")
  if (length(tab) == 0) return(0)
  max(prop.table(tab)) * 100
})

resum_pareto <- data.frame(
  variable = names(auditoria_pareto),
  percentatge_dominant = as.numeric(auditoria_pareto)
) %>% arrange(desc(percentatge_dominant))

cat("\n--- VARIABLES AMB MAJOR CONCENTRACIÓ (Pareto) ---\n")
print(head(resum_pareto, 15))


# 2.2. AUDITORIA COMPLETA DE VARIABLES CATEGÒRIQUES
df_cats <- dd[, sapply(dd, is.factor)]
for (v in names(df_cats)) {
  tab_freq <- as.data.frame(
    dd %>%
      count(!!sym(v), sort = TRUE) %>%
      mutate(Percentatge = (n / sum(n)) * 100, Acumulat = cumsum(Percentatge))
  )
  cat("\n======================================================================\n")
  cat("VARIABLE:", v, "| Nivells totals:", nrow(tab_freq), "\n")
  cat("======================================================================\n")
  print(tab_freq) 
  
  p <- ggplot(tab_freq, aes(x = reorder(!!sym(v), n), y = n)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "white") +
    coord_flip() +
    labs(title = paste("Distribució completa:", v), x = "Categories", y = "Freqüència (n)") +
    theme_minimal()
  print(p)
}


# 2.3. AUDITORIA DE NUMÈRIQUES DISCRETES
vars_num <- names(dd)[sapply(dd, is.numeric)]
valors_unics <- sapply(vars_num, function(x) length(unique(na.omit(dd[[x]]))))
candidates_a_factor <- sort(valors_unics[valors_unics < 20])

cat("\n--- NUMÈRIQUES AMB POCS VALORS ÚNICS (Candidates a Factor/Binning) ---\n")
print(candidates_a_factor)


# 2.4. DISTRIBUCIÓ DE LES NUMÈRIQUES ESTRUCTURALS
cat("\n--- DISTRIBUCIÓ DE LES NUMÈRIQUES ESTRUCTURALS ---\n")
print(table(dd$bedrooms, useNA = "always"))
print(table(dd$bathrooms, useNA = "always"))
print(table(dd$accommodates, useNA = "always"))


# ------------------------------------------------------------------------------
# BLOC 3: FEATURE EXTRACTION (Creació de variables noves)
# ------------------------------------------------------------------------------
# 3.1. EXTRACCIÓ TEMPORAL (De Dates a Dies)
data_recollida <- max(dd$last_review, na.rm = TRUE)

dd <- dd %>%
  mutate(
    dies_antiguitat_listing = as.numeric(difftime(data_recollida, first_review, units = "days")),
    dies_recencia_review    = as.numeric(difftime(data_recollida, last_review, units = "days")),
    dies_antiguitat_host    = as.numeric(difftime(data_recollida, host_since, units = "days"))
  ) %>%
  select(-first_review, -last_review, -host_since) 

cat("\n✅ Dates transformades i originals eliminades.\n")

# 3.2. EXTRACCIÓ DE TEXT: AMENITY SCORING (Índex d'equipament)
dd <- dd %>%
  mutate(
    amenities_score = if_else(
      amenities == "[]" | amenities == "" | is.na(amenities), 
      0, 
      str_count(amenities, ",") + 1
    )
  )

# 3.3. EXTRACCIÓ DE NEGOCI (De numèric a Categoria)
dd <- dd %>%
  mutate(
    host_type = if_else(calculated_host_listings_count > 1, "Professional", "Particular"),
    host_type = as.factor(host_type)
  )

# 3.4. EXTRACCIÓ NUMÈRICA I RÀTIOS
dd <- dd %>%
  mutate(
    beds_per_bedroom = if_else(bedrooms == 0 | is.na(bedrooms), beds, beds / bedrooms)
  )

# 3.5. EXTRACCIÓ GEOESPACIAL (Distància al centre d'Amsterdam)
lat_centre_ams <- 52.3731
lon_centre_ams <- 4.8922
dd <- dd %>%
  mutate(
    distancia_centre_km = sqrt(((latitude - lat_centre_ams) * 111)^2 + 
                                 ((longitude - lon_centre_ams) * 67.7)^2)
  )


# 4.1. TRANSFORMACIÓ DE CATEGORIES (BINNING)
# Reduïm de 63 a 11 nivells de 'property_type' per guanyar parsimònia.
dd <- dd %>%
  mutate(property_type = fct_lump_n(property_type, n = 10, other_level = "Altres_Propietats"))

# Passem de 4 a 3 nivells a 'room_type' per garantir estabilitat estadística.
dd <- dd %>%
  mutate(room_type = fct_collapse(room_type,
                                  "Entire home/apt" = "Entire home/apt",
                                  "Private room"    = "Private room",
                                  "Other/Shared"    = c("Hotel room", "Shared room")
  ))
cat("\n✅ Binning completat. Nivells de property_type:", nlevels(dd$property_type), "\n")

# 4.2. TRACTAMENT DE VALORS FALTANTS ESTRUCTURALS (Pisos Nous)
# Crea una discontinuïtat deliberada que el model pot identificar.
dd$dies_antiguitat_listing[is.na(dd$dies_antiguitat_listing)] <- -1
dd$dies_recencia_review[is.na(dd$dies_recencia_review)]       <- -1



# ==============================================================================
# BLOC 4.3, 4.4 i 4.5: TRANSFORMACIONS I EXPORTACIÓ A PDF
# ==============================================================================
cat("\nGenerant el PDF amb tots els gràfics...\n")

# 1. OBRIM EL PDF (Especificant la ruta i les mides)
pdf("../reports/distribucions_abans_i_despres.pdf", width = 14, height = 8)

# ------------------------------------------------------------------------------
# A. GRÀFICS PRE-TRANSFORMACIÓ (Blaus)
# ------------------------------------------------------------------------------
vars_num_actuals <- names(dd)[sapply(dd, is.numeric)]
grups_pre <- split(vars_num_actuals, ceiling(seq_along(vars_num_actuals) / 9))

for (i in seq_along(grups_pre)) {
  p <- dd %>%
    select(all_of(grups_pre[[i]])) %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "#545454", color = "white") +
    facet_wrap(~name, scales = "free", ncol = 3) +
    labs(title = paste0("PRE-TRANSFORMACIÓ - Pàgina ", i, "/", length(grups_pre)),
         subtitle = "Dades brutes abans d'aplicar logaritmes",
         x = "Valor", y = "Freqüència") +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"))
  print(p)
}

# ------------------------------------------------------------------------------
# B. TRANSFORMACIONS AUTOMÀTIQUES (Logaritmes i Categories)
# ------------------------------------------------------------------------------
vars_positives <- c(
  "price", "dies_antiguitat_listing", "dies_recencia_review", 
  "estimated_revenue_l365d", "estimated_occupancy_l365d",
  "availability_30", "availability_60", "availability_90", "availability_eoy",
  "minimum_nights", "maximum_nights", "minimum_minimum_nights", "maximum_minimum_nights",
  "minimum_maximum_nights", "maximum_maximum_nights", "minimum_nights_avg_ntm", "maximum_nights_avg_ntm",
  "number_of_reviews", "number_of_reviews_l30d", "number_of_reviews_ltm", "number_of_reviews_ly", "reviews_per_month",
  "host_listings_count", "host_total_listings_count", "calculated_host_listings_count", 
  "calculated_host_listings_count_entire_homes", "calculated_host_listings_count_private_rooms", "calculated_host_listings_count_shared_rooms"
)

vars_log_reals <- intersect(vars_positives, names(dd))

# Apliquem log1p(pmax(x, 0))
dd <- dd %>%
  mutate(across(all_of(vars_log_reals), .fns = list(log = ~log1p(pmax(., 0))), .names = "log_{.col}"))

# Categories per a asimetries negatives
if("host_acceptance_rate" %in% names(dd)) dd <- dd %>% mutate(cat_acceptance_rate = case_when(host_acceptance_rate == 1.0 ~ "Total (100%)", host_acceptance_rate >= 0.8 ~ "Selectiu", TRUE ~ "Molt Restrictiu"))
if("host_response_rate" %in% names(dd)) dd <- dd %>% mutate(cat_response_rate = case_when(host_response_rate == 1.0 ~ "Immediata (100%)", host_response_rate >= 0.9 ~ "Alta", TRUE ~ "Deficient"))

vars_reviews <- c("review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness", "review_scores_checkin", "review_scores_communication", "review_scores_location", "review_scores_value")
vars_reviews_reals <- intersect(vars_reviews, names(dd))

if(length(vars_reviews_reals) > 0) {
  dd <- dd %>%
    mutate(across(all_of(vars_reviews_reals), 
                  ~ case_when(. == -1 ~ "Nou (Sense Ressenyes)", . >= 4.8 ~ "Excel·lent (Top)", . >= 4.5 ~ "Estàndard", TRUE ~ "Millorable / Risc"),
                  .names = "cat_{.col}"))
}

vars_negatives_originals <- c("host_acceptance_rate", "host_response_rate", vars_reviews_reals)
dd <- dd %>% mutate(across(starts_with("cat_"), as.factor)) %>% select(-any_of(vars_negatives_originals))


# ------------------------------------------------------------------------------
# C. GRÀFICS POST-TRANSFORMACIÓ (Verds)
# ------------------------------------------------------------------------------
vars_num_post <- names(dd)[sapply(dd, is.numeric)]
grups_post <- split(vars_num_post, ceiling(seq_along(vars_num_post) / 9))

for (i in seq_along(grups_post)) {
  p <- dd %>%
    select(all_of(grups_post[[i]])) %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "#fd6064", color = "white") +
    facet_wrap(~name, scales = "free", ncol = 3) +
    labs(title = paste0("POST-TRANSFORMACIÓ - Pàgina ", i, "/", length(grups_post)),
         subtitle = "Efecte del logaritme en asimetries i cues",
         x = "Valor", y = "Freqüència") +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"))
  print(p)
}

# 2. TANQUEM EL PDF (Molt important, si no l'arxiu queda corrupte!)
dev.off()

cat("✅ PDF creat amb èxit! El trobaràs a la carpeta 'reports' amb el nom 'distribucions_abans_i_despres.pdf'\n")




# ==============================================================================
# BLOC 5: FEATURE SELECTION 
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.0 CONFIGURACIÓ GENERAL
# ------------------------------------------------------------------------------
dd_original <- dd

# EVITEM TRAMPES AL MODEL: Excloem l'ID i el preu original, el target és log_price
dd_fs <- dd %>% select(-id, -price)

formula_model <- log_price ~ .
target_name <- "log_price"

# Llindars
llindar_pareto       <- 98      
llindar_cor          <- 0.90    
llindar_xi           <- 0.01    
llindar_cramer       <- 0.80    
llindar_prop_nzv_num <- 10      
set.seed(123)

resum_feature_selection <- list()

# ------------------------------------------------------------------------------
# FUNCIONS AUXILIARS
# ------------------------------------------------------------------------------
calc_xi <- function(x, y) {
  idx <- complete.cases(x, y)
  x <- x[idx]
  y <- y[idx]
  if (length(x) < 10) return(NA_real_)
  if (length(unique(x)) < 2) return(NA_real_)
  if (length(unique(y)) < 2) return(NA_real_)
  
  resultat <- tryCatch({
    XICOR::xicor(x, y)
  }, error = function(e) { NA })
  
  if (all(is.na(resultat))) return(NA_real_)
  if (is.list(resultat) && "xi" %in% names(resultat)) return(as.numeric(resultat$xi))
  suppressWarnings(as.numeric(resultat)[1])
}

calc_cramers_v <- function(x, y) {
  idx <- complete.cases(x, y)
  x <- x[idx]
  y <- y[idx]
  if (length(x) == 0) return(NA_real_)
  if (length(unique(x)) < 2) return(NA_real_)
  if (length(unique(y)) < 2) return(NA_real_)
  
  tab <- table(x, y)
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
  
  out <- tryCatch({
    rcompanion::cramerV(tab)
  }, error = function(e) { NA_real_ })
  as.numeric(out)
}

# ------------------------------------------------------------------------------
# 5.1 VARIANÇA / QUASI CONSTANTS EN NUMÈRIQUES (nearZeroVar)
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("5.1 VARIANÇA / NEAR ZERO VARIANCE EN NUMÈRIQUES\n")
cat("============================================================\n")

vars_num_pred <- setdiff(names(dd_fs)[sapply(dd_fs, is.numeric)], target_name)
resum_nzv_num <- tibble()
vars_nzv_num_candidates <- character(0)

if (length(vars_num_pred) > 0) {
  dd_num <- dd_fs[, vars_num_pred, drop = FALSE]
  nzv_metrics <- caret::nearZeroVar(dd_num, saveMetrics = TRUE)
  
  resum_nzv_num <- nzv_metrics %>%
    rownames_to_column("variable") %>%
    as_tibble() %>%
    mutate(percentUnique = as.numeric(percentUnique), freqRatio = as.numeric(freqRatio)) %>%
    arrange(desc(nzv), desc(zeroVar), percentUnique)
  
  vars_nzv_num_candidates <- resum_nzv_num %>%
    filter(nzv == TRUE | zeroVar == TRUE | percentUnique <= llindar_prop_nzv_num) %>%
    pull(variable)
  
  cat("\nTaula nearZeroVar per numèriques:\n")
  print(resum_nzv_num)
  
  p_nzv_unique <- resum_nzv_num %>%
    ggplot(aes(x = reorder(variable, percentUnique), y = percentUnique, fill = nzv)) +
    geom_col() +
    coord_flip() +
    geom_hline(yintercept = llindar_prop_nzv_num, linetype = "dashed", color = "red") +
    labs(title = "Percentatge de valors únics en predictors numèrics", x = "Variable", y = "% valors únics") +
    theme_minimal()
  print(p_nzv_unique)
}

# ------------------------------------------------------------------------------
# 5.2 PARETO / QUASI CONSTANTS EN CATEGÒRIQUES
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("5.2 PARETO / QUASI CONSTANTS EN CATEGÒRIQUES\n")
cat("============================================================\n")

vars_cat <- names(dd_fs)[sapply(dd_fs, function(x) is.factor(x) || is.character(x))]

resum_pareto_fs <- tibble(
  variable = vars_cat,
  percentatge_dominant = sapply(vars_cat, function(v) {
    tab <- table(dd_fs[[v]], useNA = "no")
    if (length(tab) == 0) return(NA_real_)
    100 * max(prop.table(tab))
  })
) %>%
  mutate(variabilitat = 100 - percentatge_dominant) %>%
  arrange(percentatge_dominant)

vars_pareto_candidates <- resum_pareto_fs %>%
  filter(percentatge_dominant >= llindar_pareto) %>%
  pull(variable)

cat("\nTaula de variabilitat (Pareto):\n")
print(resum_pareto_fs)

if (nrow(resum_pareto_fs) > 0) {
  p_pareto <- ggplot(resum_pareto_fs, aes(x = reorder(variable, variabilitat), y = variabilitat)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Variabilitat de variables categòriques (100 - % categoria dominant)", x = "Variable", y = "Variabilitat (%)") +
    theme_minimal()
  print(p_pareto)
}

# ------------------------------------------------------------------------------
# 5.3 ASSOCIACIÓ ENTRE VARIABLES CATEGÒRIQUES (V DE CRAMER)
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("5.3 ASSOCIACIÓ ENTRE VARIABLES CATEGÒRIQUES (V DE CRAMER)\n")
cat("============================================================\n")

cramer_long <- tibble()
vars_cramer_candidates <- character(0)

if (length(vars_cat) >= 2) {
  parelles_cat <- combn(vars_cat, 2, simplify = FALSE)
  cramer_long <- map_dfr(parelles_cat, function(par) {
    tibble(var1 = par[1], var2 = par[2], cramer_v = calc_cramers_v(dd_fs[[par[1]]], dd_fs[[par[2]]]))
  }) %>% arrange(desc(cramer_v))
  
  vars_cramer_candidates <- cramer_long %>%
    filter(!is.na(cramer_v) & cramer_v >= llindar_cramer) %>%
    select(var1, var2) %>% unlist(use.names = FALSE) %>% unique()
  
  cat("\nParelles categòriques amb V de Cramer alt:\n")
  print(cramer_long %>% filter(!is.na(cramer_v) & cramer_v >= llindar_cramer))
  
  if (nrow(cramer_long) > 0) {
    p_cramer <- cramer_long %>%
      filter(!is.na(cramer_v)) %>%
      slice_head(n = min(100, nrow(.))) %>%
      ggplot(aes(x = reorder(paste(var1, var2, sep = " ~ "), cramer_v), y = cramer_v)) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      geom_hline(yintercept = llindar_cramer, linetype = "dashed", color = "red") +
      labs(title = "Associació entre variables categòriques (V de Cramer)", x = "Parella", y = "V de Cramer") +
      theme_minimal()
    print(p_cramer)
  }
}

# ------------------------------------------------------------------------------
# 5.4 CORRELACIÓ ENTRE PREDICTORS NUMÈRICS
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("5.4 CORRELACIÓ ENTRE PREDICTORS NUMÈRICS\n")
cat("============================================================\n")

cor_mat <- NULL
cor_long <- tibble()
vars_cor_candidates <- character(0)

if (length(vars_num_pred) >= 2) {
  cor_mat <- cor(dd_fs[, vars_num_pred, drop = FALSE], use = "pairwise.complete.obs")
  vars_cor_candidates <- caret::findCorrelation(cor_mat, cutoff = llindar_cor, names = TRUE)
  
  cor_long <- as.data.frame(as.table(cor_mat)) %>%
    rename(var1 = Var1, var2 = Var2, correlacio = Freq) %>%
    filter(var1 != var2) %>%
    mutate(parella = purrr::map2_chr(var1, var2, ~ paste(sort(c(.x, .y)), collapse = "___"))) %>%
    distinct(parella, .keep_all = TRUE) %>%
    mutate(abs_cor = abs(correlacio)) %>%
    arrange(desc(abs_cor))
  
  cat("\nParelles amb correlació alta:\n")
  print(cor_long %>% filter(abs_cor > llindar_cor) %>% select(var1, var2, correlacio))
  
  corrplot::corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.7, number.cex = 0.6)
  
  p_cor <- cor_long %>%
    slice_head(n = min(20, nrow(cor_long))) %>%
    ggplot(aes(x = reorder(paste(var1, var2, sep = " ~ "), abs_cor), y = abs_cor)) +
    geom_col(fill = "tomato") +
    coord_flip() +
    geom_hline(yintercept = llindar_cor, linetype = "dashed", color = "red") +
    labs(title = "Magnitud de correlació entre predictors numèrics", x = "Parella", y = "|correlació|") +
    theme_minimal()
  print(p_cor)
}

# ------------------------------------------------------------------------------
# 5.5 COMBINACIONS LINEALS EXACTES (findLinearCombos)
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("5.5 COMBINACIONS LINEALS EXACTES (findLinearCombos)\n")
cat("============================================================\n")

combos <- NULL
vars_linear_candidates <- character(0)

if (length(vars_num_pred) >= 2) {
  dd_num_complete <- dd_fs[, vars_num_pred, drop = FALSE] %>% drop_na()
  if (nrow(dd_num_complete) > 10) {
    combos <- tryCatch({ caret::findLinearCombos(dd_num_complete) }, error = function(e) { NULL })
    if (!is.null(combos) && !is.null(combos$remove)) {
      vars_linear_candidates <- colnames(dd_num_complete)[combos$remove]
    }
    cat("\nResultat de findLinearCombos:\n")
    print(combos)
  }
}

# ------------------------------------------------------------------------------
# 5.6 COEFICIENT XI AMB LA VARIABLE RESPOSTA
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("5.6 XI CORRELATION AMB LA VARIABLE RESPOSTA\n")
cat("============================================================\n")

xi_scores <- sapply(vars_num_pred, function(v) calc_xi(dd_fs[[v]], dd_fs[[target_name]]))

resum_xi <- tibble(variable = vars_num_pred, xi = as.numeric(xi_scores)) %>% arrange(desc(xi))
vars_xi_candidates <- resum_xi %>% filter(is.na(xi) | xi < llindar_xi) %>% pull(variable)

cat("\nTaula Xi:\n")
print(resum_xi)

if (nrow(resum_xi) > 0) {
  p_xi <- ggplot(resum_xi, aes(x = reorder(variable, xi), y = xi)) +
    geom_col(fill = "darkgreen", na.rm = TRUE) +
    coord_flip() +
    geom_hline(yintercept = llindar_xi, linetype = "dashed", color = "red") +
    labs(title = paste("Coeficient Xi amb la resposta:", target_name), x = "Variable", y = "Xi") +
    theme_minimal()
  print(p_xi)
}

# ------------------------------------------------------------------------------
# 5.7 RANDOM FOREST IMPORTANCE
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("5.7 RANDOM FOREST IMPORTANCE\n")
cat("============================================================\n")

dd_rf <- dd_fs %>% mutate(across(where(is.character), as.factor))
nivells_factors <- sapply(dd_rf, function(x) if (is.factor(x)) nlevels(x) else NA_integer_)
vars_massa_nivells <- names(nivells_factors)[!is.na(nivells_factors) & nivells_factors > 53]

cat("\nVariables excloses TEMPORALMENT del RF per massa nivells (>53):\n")
print(vars_massa_nivells)

dd_rf_model <- dd_rf %>% select(-any_of(vars_massa_nivells)) %>% drop_na()

if (nrow(dd_rf_model) > 10) {
  rf_model <- randomForest::randomForest(formula_model, data = dd_rf_model, importance = TRUE, ntree = 100)
  imp_rf <- as.data.frame(randomForest::importance(rf_model, type = 1))
  imp_rf$variable <- rownames(imp_rf)
  rownames(imp_rf) <- NULL
  names(imp_rf)[1] <- "importance"
  imp_rf <- imp_rf %>% arrange(desc(importance))
  
  llindar_rf <- median(imp_rf$importance, na.rm = TRUE)
  vars_rf_candidates <- imp_rf %>% filter(importance <= llindar_rf) %>% pull(variable)
  
  varImpPlot(rf_model, main = "Variable Importance - Random Forest")
  
  p_rf <- ggplot(imp_rf, aes(x = reorder(variable, importance), y = importance)) +
    geom_col(fill = "purple") +
    coord_flip() +
    geom_hline(yintercept = llindar_rf, linetype = "dashed", color = "red") +
    labs(title = "Importància de variables segons Random Forest", x = "Variable", y = "Importance") +
    theme_minimal()
  print(p_rf)
} else {
  vars_rf_candidates <- character(0)
  cat("\nNo hi ha prou observacions completes per entrenar el Random Forest.\n")
}

# ------------------------------------------------------------------------------
# 5.8 RESUM FINAL PER A LA DECISIÓ
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("5.8 RESUM FINAL DE CANDIDATES A ELIMINAR\n")
cat("============================================================\n")

resum_decisions <- tibble(
  variable = unique(c(vars_nzv_num_candidates, vars_pareto_candidates, vars_cramer_candidates, 
                      vars_cor_candidates, vars_linear_candidates, vars_xi_candidates, 
                      vars_rf_candidates, vars_massa_nivells))
) %>%
  mutate(
    nzv_numerica          = variable %in% vars_nzv_num_candidates,
    pareto_quasi_constant = variable %in% vars_pareto_candidates,
    cramer_alt            = variable %in% vars_cramer_candidates,
    alta_correlacio       = variable %in% vars_cor_candidates,
    combo_lineal          = variable %in% vars_linear_candidates,
    xi_baix               = variable %in% vars_xi_candidates,
    rf_baixa_importancia  = variable %in% vars_rf_candidates,
    exclosa_tecnic_rf     = variable %in% vars_massa_nivells
  )

cat("\nTaula resum de criteris per variable:\n")
print(resum_decisions, n = Inf)

if (nrow(resum_decisions) > 0) {
  resum_long <- resum_decisions %>%
    pivot_longer(-variable, names_to = "criteri", values_to = "marcada")
  
  p_resum <- ggplot(resum_long, aes(x = criteri, y = reorder(variable, variable), fill = marcada)) +
    geom_tile(color = "white") +
    labs(title = "Resum de variables candidates segons cada criteri", x = "Criteri", y = "Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_resum)
}

# ------------------------------------------------------------------------------
# 5.9 DECISIÓ FINAL MANUAL
# ------------------------------------------------------------------------------
vars_finals_a_eliminar <- c(
  
  # DISPONIBILITAT (esborrem redundants i les seves versions log)
  "availability_30", "log_availability_30",
  "availability_60", "log_availability_60",
  "availability_90", "log_availability_90",
  "availability_eoy", "log_availability_eoy",
  
  # RESSENYES (Categories altament relacionades o redundants)
  "cat_review_scores_accuracy",
  "cat_review_scores_cleanliness",
  "cat_review_scores_checkin",
  "cat_review_scores_communication",
  "cat_review_scores_location",
  "cat_review_scores_value",
  
  # NITS (Variables derivades i els seus logs)
  "minimum_minimum_nights", "log_minimum_minimum_nights",
  "minimum_maximum_nights", "log_minimum_maximum_nights",
  "minimum_nights_avg_ntm", "log_minimum_nights_avg_ntm",
  "maximum_minimum_nights", "log_maximum_minimum_nights",
  "maximum_maximum_nights", "log_maximum_maximum_nights",
  "maximum_nights_avg_ntm", "log_maximum_nights_avg_ntm",
  
  # RESSENYES COMPTADORS REDUNDANTS
  "number_of_reviews_l30d", "log_number_of_reviews_l30d",
  "number_of_reviews_ltm", "log_number_of_reviews_ltm",
  "number_of_reviews_ly", "log_number_of_reviews_ly",
  
  # HOST LISTINGS REDUNDANTS 
  "host_total_listings_count", "log_host_total_listings_count",
  "calculated_host_listings_count", "log_calculated_host_listings_count",
  "calculated_host_listings_count_private_rooms", "log_calculated_host_listings_count_private_rooms",
  "calculated_host_listings_count_shared_rooms", "log_calculated_host_listings_count_shared_rooms",
  "host_listings_count", "log_host_listings_count",
  "calculated_host_listings_count_entire_homes", "log_calculated_host_listings_count_entire_homes",
  
  # PARETO (Poca variabilitat)
  "host_has_profile_pic", "host_identity_verified"
)

# Eliminem de 'dd_original' per conservar la columna de 'price' sense log per al clustering/profiling
dd_final_fs <- dd_original %>%
  select(-any_of(vars_finals_a_eliminar))

cat("\n============================================================\n")
cat("5.9 DECISIÓ FINAL MANUAL\n")
cat("============================================================\n")
cat("\nVariables finals eliminades manualment:\n")
print(vars_finals_a_eliminar)

cat("\nDimensions del dataset després de la feature selection final:\n")
print(dim(dd_final_fs))

names(dd_final_fs)

saveRDS(dd_final_fs, "../data/dataset_feature_selection_final.rds")
write.csv(dd_final_fs, file = "../data/dataset_feature_selection_final.csv", row.names = FALSE)
cat("\nFitxer generat: dataset_feature_selection_final.rds\n")
