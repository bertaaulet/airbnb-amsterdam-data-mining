# ==============================================================================
# PREPROCESSING: IMPUTACIÓ DE DADES MANCANTS
# ------------------------------------------------------------------------------
# OBJECTIU:
#   1) Detectar i descriure els missings
#   2) Caracteritzar-ne el mecanisme (MCAR / MAR / sospita MNAR)
#   3) Aplicar les 3 estratègies de la teoria:
#        - Estratègia 1: eliminació
#        - Estratègia 2: imputació simple (mitjana/mediana/moda)
#        - Estratègia 3: imputació avançada (KNN / MICE / MIMMI / missForest)
#   4) Comparar before vs after per decidir què té més sentit
# ==============================================================================


# ==============================================================================
# 0. LLIBRERIES
# ==============================================================================
library(naniar)
library(tidyverse)
library(dplyr)
library(inspectdf)
library(VIM)         # KNN amb dades mixtes
library(mice)        # MICE
library(missForest)  # missForest = imputació amb Random Forests
library(visdat)      # visualització de missings
library(Hmisc)       # eines addicionals
library(forcats)     # tractament explícit d'NAs en factors
library(cluster)     # necessari per MIMMI (daisy)
library(StatMatch)   # necessari per MIMMI
library(caret)       # nearZeroVar


# ==============================================================================
# 1. CÀRREGA DE DADES
# ==============================================================================
dd <- readRDS("../data/dades_profiling.rds")

View(dd)
head(dd)
str(dd)

vis_miss

# ==============================================================================
# 2. TIPUS DE DADES
# ==============================================================================
inspect_types(dd) %>% show_plot()

# Llista de variables textuals / metadades
# bathrooms_text es manté de moment perquè la necessitem per reconstruir bathrooms
vars_text <- c(
  "listing_url", "name", "description", "neighborhood_overview", 
  "picture_url", "host_url", "host_name", "host_location", 
  "host_about", "host_thumbnail_url", "host_picture_url", 
  "host_verifications", "bathrooms_text", "amenities", "license"
)

dd <- dd %>%
  mutate(across(all_of(vars_text), as.character))

inspect_types(dd) %>% show_plot()


# ==============================================================================
# 3. ANÀLISI DEL PERCENTATGE DE MISSINGS
# ==============================================================================
missings <- miss_var_summary(dd)
View(missings)

# ------------------------------------------------------------------------------
# DETECCIÓ DE PATRONS DE "MISSINGNESS" SISTEMÀTICA
# ------------------------------------------------------------------------------
ids_corruptes <- dd %>%
  filter(is.na(host_listings_count) | is.na(minimum_minimum_nights)) %>%
  pull(id)

cat("S'han detectat", length(ids_corruptes), "registres amb dades estructurals corruptes.\n")

# ------------------------------------------------------------------------------
# JUSTIFICACIÓ DE LA DECISIÓ: ELIMINACIÓ (LISTWISE DELETION)
# ------------------------------------------------------------------------------
dd <- dd %>%
  filter(!id %in% ids_corruptes)

gg_miss_var(dd) + 
  labs(title = "Quantitat de valors faltants per variable",
       subtitle = "Subconjunt d'anàlisi",
       x = "Variables",
       y = "Nombre de valors faltants (NA)") +
  theme_minimal()

dades_missings <- miss_var_summary(dd) %>% 
  filter(n_miss > 0)

print(dades_missings, n = Inf)


# Eliminació de columnes redundants amb alt % de missings
# Justificació: >50% de buits i informació ja coberta per neighbourhood_cleansed
dd <- dd %>%
  select(-neighbourhood, -host_neighbourhood)

# Variable de text: tractament explícit NS/NC
dd <- dd %>%
  mutate(neighborhood_overview = as.character(neighborhood_overview)) %>% 
  mutate(neighborhood_overview = replace_na(neighborhood_overview, "Sense descripció del barri"))

cat("Variables restants:", ncol(dd), "\n")
summary(dd$neighborhood_overview == "Sense descripció del barri")


# ==============================================================================
# 4. AUDITORIA SISTEMÀTICA DEL BLOC DE RESSENYES
# ==============================================================================
vars_ressenyes <- c(
  "review_scores_cleanliness", "first_review", "last_review", 
  "review_scores_rating", "review_scores_accuracy", "review_scores_checkin", 
  "review_scores_communication", "review_scores_location", 
  "review_scores_value", "reviews_per_month"
)

comprovacio_massiva <- sapply(vars_ressenyes, function(columna) {
  dd %>%
    filter(is.na(.data[[columna]])) %>%
    summarise(max_reviews = max(number_of_reviews, na.rm = TRUE)) %>%
    pull(max_reviews)
})

resultats_auditoria <- data.frame(
  Variable = vars_ressenyes,
  Max_Reviews_en_NAs = comprovacio_massiva
)

print(resultats_auditoria)

# Gestió conservadora del bloc de reviews
dd <- dd %>%
  mutate(review_scores_cleanliness = if_else(
    is.na(review_scores_cleanliness) & number_of_reviews > 0,
    median(review_scores_cleanliness, na.rm = TRUE),
    review_scores_cleanliness
  )) %>%
  mutate(listing_is_new = if_else(number_of_reviews == 0, 1, 0))


# ==============================================================================
# 5. RECONSTRUIR BATHROOMS A PARTIR DE BATHROOMS_TEXT
# ==============================================================================

# Creem una taula de freqüències per veure absolutament totes les categories
taula_banys <- dd %>%
  count(bathrooms_text, sort = TRUE)

print(as.data.frame(taula_banys))

dd <- dd %>%
  mutate(
    # 1. Passem tot a minúscules per evitar problemes d'escriptura
    bath_text_low = str_to_lower(bathrooms_text),
    
    # 2. Extraiem el número amb Regex (agafarà el 1, 1.5, 2, etc.)
    bath_num_extraient = as.numeric(str_extract(bath_text_low, "[0-9\\.]+")),
    
    # 3. EXCEPCIÓ LÒGICA: Si hi diu "half-bath", forcem un 0.5 (sobreescrivint el NA)
    bath_num_extraient = case_when(
      str_detect(bath_text_low, "half-bath") ~ 0.5,
      TRUE ~ bath_num_extraient
    ),
    
    # 4. Omplim els NAs de la columna original 'bathrooms' amb el valor netejat
    bathrooms = if_else(is.na(bathrooms), bath_num_extraient, bathrooms),
    
    # 5. Creem la dummy per "shared" (detectarà "shared bath", "shared half-bath", etc.)
    bath_is_shared = if_else(str_detect(bath_text_low, "shared"), 1, 0)
  ) %>%
  # Netegem les columnes temporals i la de text (ja hem extret tot el valor!)
  select(-bath_text_low, -bath_num_extraient, -bathrooms_text)

# Comprovació final de qualitat
cat("NAs restants a bathrooms:", sum(is.na(dd$bathrooms)), "\n")

# ==============================================================================
# 6. SEPARACIÓ DEL DATASET (SPLIT)
# ==============================================================================
# Ara bathrooms_text ja no existeix
vars_text <- c(
  "listing_url", "name", "description", "neighborhood_overview", 
  "picture_url", "host_url", "host_name", "host_location", 
  "host_about", "host_thumbnail_url", "host_picture_url", 
  "host_verifications", "amenities", "license"
)

dd_text <- dd %>%
  select(id, all_of(vars_text))

dd_imputar <- dd %>%
  select(-all_of(vars_text))

cat("Dimensions dd_text:   ", dim(dd_text)[1], "files x", dim(dd_text)[2], "columnes\n")
cat("Dimensions dd_imputar:", dim(dd_imputar)[1], "files x", dim(dd_imputar)[2], "columnes\n")


# ==============================================================================
# 7. TIPOLOGIA DE MISSINGS
# ==============================================================================
vis_miss(dd_imputar, warn_large_data = FALSE) +
  ggtitle("Mapa de Missings: On es concentren els buits?")

visdat::vis_miss(dd_imputar, warn_large_data = FALSE)

# ------------------------------------------------------------------------------
# PERFIL GLOBAL DE MISSINGS PER INDIVIDU
# ------------------------------------------------------------------------------
dd_imputar <- dd_imputar %>%
  mutate(n_miss_individu = rowSums(is.na(.)))

table(dd_imputar$n_miss_individu)

ggplot(dd_imputar, aes(x = n_miss_individu)) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
  labs(
    title = "Distribució del nombre de missings per individu",
    x = "Nombre de missings",
    y = "Freqüència"
  ) +
  theme_minimal()

missings_per_individu = miss_case_summary(dd_imputar)
view(missings_per_individu)
miss_var_summary(dd_imputar)


# ==============================================================================
# 8. TEST DE LITTLE (Multivariant per a variables numèriques)
# ==============================================================================
df_little <- dd_imputar %>%
  select(
    price, beds, bathrooms, bedrooms,
    accommodates,
    host_response_rate, host_acceptance_rate
  ) %>%
  mutate(across(everything(), as.numeric))

resultat_little <- mcar_test(df_little)

cat("\n--- RESULTATS TEST DE LITTLE ---\n")
print(resultat_little)

if (!is.null(resultat_little$p.value)) {
  if (resultat_little$p.value < 0.05) {
    cat("Interpretació: p.value < 0.05 -> rebutgem H0. El bloc NO és MCAR; probablement MAR o MNAR.\n")
  } else {
    cat("Interpretació: p.value >= 0.05 -> no rebutgem H0. El bloc és compatible amb MCAR.\n")
  }
} else {
  cat("Interpretació: el test no ha retornat p.value de manera clara. Revisar estructura del subconjunt numèric.\n")
}


# ==============================================================================
# 9. TEST DE CHI-QUADRAT (Per a variables categòriques / evidència de MAR)
# ==============================================================================
dd_imputar <- dd_imputar %>%
  mutate(
    source = as.factor(source),
    room_type = as.factor(room_type),
    property_type = as.factor(property_type),
    instant_bookable = as.factor(instant_bookable),
    host_is_superhost = as.factor(host_is_superhost),
    host_has_profile_pic = as.factor(host_has_profile_pic),
    host_identity_verified = as.factor(host_identity_verified),
    bath_is_shared = as.factor(bath_is_shared),
    listing_is_new = as.factor(listing_is_new)
  )

executa_chisq_missing <- function(data, var_missing, vars_explicatives) {
  df_aux <- data %>%
    mutate(miss_ind = factor(if_else(is.na(.data[[var_missing]]), "Missing", "Observed")))
  
  resultats <- lapply(vars_explicatives, function(v) {
    taula <- table(df_aux$miss_ind, df_aux[[v]], useNA = "no")
    
    if (nrow(taula) < 2 || ncol(taula) < 2) {
      return(data.frame(
        variable_missing = var_missing,
        variable_explicativa = v,
        p_value = NA_real_,
        interpretacio = "Taula no vàlida per al test"
      ))
    }
    
    test <- suppressWarnings(chisq.test(taula))
    
    interpretacio <- ifelse(
      is.na(test$p.value),
      "No interpretable",
      ifelse(test$p.value < 0.05, "Associació significativa -> evidència de MAR",
             "No hi ha associació clara -> compatible amb MCAR")
    )
    
    data.frame(
      variable_missing = var_missing,
      variable_explicativa = v,
      p_value = test$p.value,
      interpretacio = interpretacio
    )
  })
  
  bind_rows(resultats)
}

vars_explicatives_cat <- c(
  "room_type", "property_type", "instant_bookable",
  "host_is_superhost", "host_identity_verified",
  "bath_is_shared", "listing_is_new", "source"
)

res_chi_price <- executa_chisq_missing(dd_imputar, "price", vars_explicatives_cat)
res_chi_beds  <- executa_chisq_missing(dd_imputar, "beds", vars_explicatives_cat)
res_chi_host_response <- executa_chisq_missing(dd_imputar, "host_response_rate", vars_explicatives_cat)
res_chi_bathrooms <- executa_chisq_missing(dd_imputar, "bathrooms", vars_explicatives_cat)

cat("\n--- RESULTATS CHI-QUADRAT: PRICE ---\n")
print(res_chi_price)

cat("\n--- RESULTATS CHI-QUADRAT: BEDS ---\n")
print(res_chi_beds)

cat("\n--- RESULTATS CHI-QUADRAT: HOST_RESPONSE_RATE ---\n")
print(res_chi_host_response)

cat("\n--- RESULTATS CHI-QUADRAT: BATHROOMS ---\n")
print(res_chi_bathrooms)


# ==============================================================================
# 10. PREPARACIÓ DE VARIABLES PER IMPUTAR
# ==============================================================================
# Elimina la columna de nombre de missings
dd_imputar_base <- dd_imputar %>%
  select(-n_miss_individu)

dd_base <- dd_imputar_base



# funcio que detecta si conté dates
es_date <- function(x) inherits(x, "Date")

# funció per computar la moda
Mode <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(NA)
  tab <- table(ux)
  names(tab)[which.max(tab)]
}

# funcio perque els models d'imputacio no fallin:
# eliminació de id i conversio a numeriques o factors
prepara_per_model <- function(df, treure_id = TRUE) {
  out <- df
  
  if (treure_id && "id" %in% names(out)) {
    out <- out %>% select(-id)
  }
  
  out <- out %>%
    mutate(across(where(es_date), as.numeric)) %>%
    mutate(across(where(is.logical), as.factor)) %>%
    mutate(across(where(is.character), as.factor))
  
  return(out)
}


# ==============================================================================
# 11. ESTRATÈGIA 1: ELIMINACIÓ
# ==============================================================================
dd_strategy1_global <- dd_base %>%
  drop_na()

cat("\n--- ESTRATÈGIA 1: ELIMINACIÓ GLOBAL ---\n")
cat("Files abans:", nrow(dd_base), "\n")
cat("Files després de drop_na():", nrow(dd_strategy1_global), "\n")
cat("Percentatge conservat:", round(100 * nrow(dd_strategy1_global) / nrow(dd_base), 2), "%\n")

dd_strategy1_price <- dd_base %>% filter(!is.na(price))
dd_strategy1_beds <- dd_base %>% filter(!is.na(beds))
dd_strategy1_bathrooms <- dd_base %>% filter(!is.na(bathrooms))
dd_strategy1_bedrooms <- dd_base %>% filter(!is.na(bedrooms))
dd_strategy1_host_response_rate <- dd_base %>% filter(!is.na(host_response_rate))
dd_strategy1_host_acceptance_rate <- dd_base %>% filter(!is.na(host_acceptance_rate))

cat("\n--- PÈRDUA DE FILES PER VARIABLE (Estratègia 1) ---\n")
cat("price:", nrow(dd_base) - nrow(dd_strategy1_price), "files eliminades\n")
cat("beds:", nrow(dd_base) - nrow(dd_strategy1_beds), "files eliminades\n")
cat("bathrooms:", nrow(dd_base) - nrow(dd_strategy1_bathrooms), "files eliminades\n")
cat("bedrooms:", nrow(dd_base) - nrow(dd_strategy1_bedrooms), "files eliminades\n")
cat("host_response_rate:", nrow(dd_base) - nrow(dd_strategy1_host_response_rate), "files eliminades\n")
cat("host_acceptance_rate:", nrow(dd_base) - nrow(dd_strategy1_host_acceptance_rate), "files eliminades\n")


# ==============================================================================
# 12. ESTRATÈGIA 2: IMPUTACIÓ SIMPLE
# ==============================================================================
# Aquesta variant garanteix un dataset sense missings (zero NAs) mitjançant
# l'ús d'estadístics de tendència central:
#   - Variables Numèriques: Imputació per la MEDIANA, per ser un estimador 
#     robust davant valors atípics (outliers) com preus extrems.
#   - Variables Qualitatives: Imputació per la MODA (valor més freqüent) per
#     mantenir la coherència de les categories del dataset.
# Nota: És un mètode ràpid però pot reduir artificialment la variància.

dd_strategy2_simple <- dd_base

for (col in names(dd_strategy2_simple)) {
  if (any(is.na(dd_strategy2_simple[[col]]))) {
    
    if (is.numeric(dd_strategy2_simple[[col]])) {
      dd_strategy2_simple[[col]][is.na(dd_strategy2_simple[[col]])] <- 
        median(dd_strategy2_simple[[col]], na.rm = TRUE)
      
    } else if (is.logical(dd_strategy2_simple[[col]])) {
      moda <- Mode(dd_strategy2_simple[[col]])
      dd_strategy2_simple[[col]][is.na(dd_strategy2_simple[[col]])] <- as.logical(moda)
      
    } else if (is.factor(dd_strategy2_simple[[col]]) || is.character(dd_strategy2_simple[[col]])) {
      moda <- Mode(dd_strategy2_simple[[col]])
      dd_strategy2_simple[[col]][is.na(dd_strategy2_simple[[col]])] <- moda
    }
  }
}

cat("\n--- ESTRATÈGIA 2: IMPUTACIÓ SIMPLE ---\n")
cat("Missings restants després d'imputació simple:", sum(is.na(dd_strategy2_simple)), "\n")


# Aquesta variant busca preservar la naturalesa de la manca d'informació:
#   - Variables Numèriques: Es manté la imputació per la MEDIANA.
#   - Variables Qualitatives (Factors/Text): En lloc de "batejar" els buits amb
#     la moda, es crea la categoria "NS/NC" (No Sabe/No Contesta).
# Avantatge: Permet identificar si l'absència de dada té un patró informatiu o 
# significat propi (perfil del host que no omple determinats camps).

dd_strategy2_explicit_na <- dd_base

for (col in names(dd_strategy2_explicit_na)) {
  if (is.numeric(dd_strategy2_explicit_na[[col]]) && any(is.na(dd_strategy2_explicit_na[[col]]))) {
    dd_strategy2_explicit_na[[col]][is.na(dd_strategy2_explicit_na[[col]])] <- 
      median(dd_strategy2_explicit_na[[col]], na.rm = TRUE)
  }
  
  if (is.factor(dd_strategy2_explicit_na[[col]])) {
    dd_strategy2_explicit_na[[col]] <- forcats::fct_explicit_na(
      dd_strategy2_explicit_na[[col]], na_level = "NS/NC"
    )
  }
  
  if (is.character(dd_strategy2_explicit_na[[col]]) && any(is.na(dd_strategy2_explicit_na[[col]]))) {
    dd_strategy2_explicit_na[[col]] <- replace_na(dd_strategy2_explicit_na[[col]], "NS/NC")
  }
  
  if (is.logical(dd_strategy2_explicit_na[[col]]) && any(is.na(dd_strategy2_explicit_na[[col]]))) {
    moda <- Mode(dd_strategy2_explicit_na[[col]])
    dd_strategy2_explicit_na[[col]][is.na(dd_strategy2_explicit_na[[col]])] <- as.logical(moda)
  }
}

cat("Missings restants amb variant NS/NC:", sum(is.na(dd_strategy2_explicit_na)), "\n")


# ==============================================================================
# 13. ESTRATÈGIA 3A: KNN 
# ==============================================================================
# Optimitzem k independentment per a cada variable numèrica.

# 1. Preparem el dataset base (assegura't que dd_base existeix)
df_knn_final <- prepara_per_model(dd_base, treure_id = TRUE)

# 2. Identifiquem quines columnes tenen missings i quines estan completes
vars_a_imputar <- names(df_knn_final)[colSums(is.na(df_knn_final)) > 0]
vars_completes <- names(df_knn_final)[colSums(is.na(df_knn_final)) == 0]

# Valors de k a testejar segons apunts
k_proves <- c(3, 5, 7, 9)

# 3. Bucle principal per cada variable amb missings
for (v in vars_a_imputar) {
  
  # Només optimitzem amb Test KS si la variable és numèrica
  if (is.numeric(df_knn_final[[v]])) {
    cat("\nOptimitzant k per a la variable numèrica:", v, "\n")
    
    millor_k_v <- 3
    millor_ks_v <- Inf
    vals_originals <- na.omit(df_knn_final[[v]])
    
    for (k_provar in k_proves) {
      # Imputació temporal només d'aquesta variable
      temp_knn <- VIM::kNN(df_knn_final, variable = v, dist_var = vars_completes, 
                           k = k_provar, imp_var = TRUE)
      
      # Seleccionem els valors que eren missings (identificats per la columna _imp)
      nom_col_imp <- paste0(v, "_imp")
      vals_imputats <- temp_knn[[v]][temp_knn[[nom_col_imp]] == TRUE]
      
      # Si hem aconseguit imputar valors, fem el test de Kolmogorov-Smirnov
      if (length(vals_imputats) > 0) {
        ks_v <- ks.test(vals_originals, vals_imputats)$statistic
        cat("  Provant k =", k_provar, "| KS stat =", round(ks_v, 4), "\n")
        
        if (ks_v < millor_ks_v) {
          millor_ks_v <- ks_v
          millor_k_v <- k_provar
        }
      }
    }
    cat("  -> Guanyadora per a", v, ": k =", millor_k_v, "amb KS =", round(millor_ks_v, 4), "\n")
    
    # Apliquem la millor k trobada definitivament al dataset final
    df_knn_final <- VIM::kNN(df_knn_final, variable = v, dist_var = vars_completes, 
                             k = millor_k_v, imp_var = FALSE)
    
  } else {
    # Per a categòriques (factors), apliquem k=5 per defecte (mètode robust)
    cat("\nImputant variable categòrica amb k=5 per defecte:", v, "\n")
    df_knn_final <- VIM::kNN(df_knn_final, variable = v, dist_var = vars_completes, 
                             k = 5, imp_var = FALSE)
  }
}

# 4. Reconstruïm el dataset amb l'ID original
dd_strategy3_knn <- bind_cols(id = dd_base$id, df_knn_final)

cat("\n--- ESTRATÈGIA 3A FINALITZADA ---\n")
cat("Missings restants al dataset KNN:", sum(is.na(dd_strategy3_knn)), "\n")

# ==============================================================================
# 14. ESTRATÈGIA 3B: MICE
# ==============================================================================
# IMPLEMENTACIÓ NETA I ÚNICA DE MICE
#
# Quan s'intentava passar gairebé totes les variables a MICE, apareixia:
#   system is computationally singular
#
# Això es deu a:
#   - col·linearitat forta
#   - variables derivades o molt redundants
#   - patrons de missing gairebé idèntics en alguns blocs
#
# Per evitar-ho de la manera menys agressiva possible, aquí es defineix un únic
# subconjunt MICE amb variables substantivament rellevants i més estables.
#
# VARIABLES EXCLOSES DE MICE (per estabilitat):
#   - host_response_rate
#       -> es deixa fora perquè forma un bloc molt alineat amb
#          host_response_time i host_acceptance_rate, i en els intents complets
#          contribuïa a la singularitat computacional.
#   - first_review, last_review
#       -> dates de review que aquí no són imprescindibles per imputar el nucli.
#   - estimated_revenue_l365d
#       -> variable derivada i molt lligada a price / ocupació.
#   - host_listings_count, host_total_listings_count i counted-host variants
#       -> comptadors molt redundants entre si.
#   - variables geogràfiques i metadades no essencials per al model d'imputació.
#
# DECISIÓ:
#   - mantenim el nucli de variables d'allotjament, reviews, disponibilitat i host
#   - fem servir el patró clàssic dels scripts: mice(maxit=0) -> method -> complete()

dd_strategy3_mice <- dd_base

vars_mice <- c(
  "source",
  "host_is_superhost",
  "host_has_profile_pic",
  "host_identity_verified",
  "room_type",
  "property_type",
  "accommodates",
  "bathrooms",
  "bath_is_shared",
  "bedrooms",
  "beds",
  "price",
  "minimum_nights",
  "maximum_nights",
  "availability_30",
  "availability_60",
  "availability_90",
  "availability_365",
  "number_of_reviews",
  "number_of_reviews_ltm",
  "number_of_reviews_l30d",
  "number_of_reviews_ly",
  "review_scores_rating",
  "review_scores_accuracy",
  "review_scores_cleanliness",
  "review_scores_checkin",
  "review_scores_communication",
  "review_scores_location",
  "review_scores_value",
  "reviews_per_month",
  "instant_bookable",
  "listing_is_new",
  "host_response_time",
  "host_acceptance_rate"
)

vars_mice <- intersect(vars_mice, names(dd_base))
vars_excloses_mice <- setdiff(names(dd_base), c("id", vars_mice))

cat("\n--- MICE: VARIABLES EXCLOSES ---\n")
print(vars_excloses_mice)

df_mice <- dd_base %>%
  select(all_of(vars_mice)) %>%
  prepara_per_model(treure_id = FALSE)

# Eliminem només variables quasi constants, com a pas conservador d'estabilització
nzv_mice <- nearZeroVar(df_mice)
if (length(nzv_mice) > 0) {
  cat("MICE: s'eliminen", length(nzv_mice), "variables quasi constants.\n")
  df_mice <- df_mice[, -nzv_mice, drop = FALSE]
}

md.pattern(df_mice)

# Inicialització
ini_mice <- mice(df_mice, maxit = 0, printFlag = FALSE)
metodes_mice <- ini_mice$method
pred_mice <- ini_mice$predictorMatrix

# Definició de mètodes
for (col in names(df_mice)) {
  if (!any(is.na(df_mice[[col]]))) {
    metodes_mice[col] <- ""
  } else if (is.numeric(df_mice[[col]])) {
    metodes_mice[col] <- "pmm"
  } else if (is.factor(df_mice[[col]]) && nlevels(df_mice[[col]]) == 2) {
    metodes_mice[col] <- "logreg"
  } else if (is.factor(df_mice[[col]]) && nlevels(df_mice[[col]]) > 2) {
    metodes_mice[col] <- "polyreg"
  }
}

diag(pred_mice) <- 0

imp_mice <- tryCatch({
  set.seed(123)
  mice(
    df_mice,
    m = 5,
    maxit = 20,
    method = metodes_mice,
    predictorMatrix = pred_mice,
    seed = 123,
    printFlag = TRUE
  )
}, error = function(e) {
  cat("\nMICE ha fallat.\n")
  cat("Missatge d'error:", conditionMessage(e), "\n")
  return(NULL)
})

if (!is.null(imp_mice)) {
  dd_strategy3_mice_core <- complete(imp_mice, 1)
  cols_mice <- intersect(names(dd_strategy3_mice_core), names(dd_strategy3_mice))
  dd_strategy3_mice[, cols_mice] <- dd_strategy3_mice_core[, cols_mice]
}

cat("\n--- ESTRATÈGIA 3B: MICE ---\n")
cat("Missings restants al dataset complet després de MICE:", sum(is.na(dd_strategy3_mice)), "\n")
cat("Missings restants dins del subconjunt imputat amb MICE:", sum(is.na(dd_strategy3_mice[names(df_mice)])), "\n")


# ==============================================================================
# 15. ESTRATÈGIA 3C: MIMMI
# ==============================================================================
# Implementació única de MIMMI

uncompleteVar <- function(vector) {
  any(is.na(vector))
}

Mode_MIMMI <- function(x) {
  x <- as.factor(x)
  maxV <- which.max(table(x))
  return(levels(x)[maxV])
}

MiMMi <- function(data, priork = 5) {
  
  colsMiss <- which(sapply(data, uncompleteVar))
  
  if (length(colsMiss) == 0) {
    message("No missing values found")
    out <- new.env()
    out$imputedData <- data
    out$imputation <- NULL
    return(out)
  }
  
  K <- ncol(data)
  colsNoMiss <- setdiff(seq_len(K), colsMiss)
  
  if (length(colsNoMiss) == 0) {
    stop("MIMMI no es pot aplicar: no hi ha cap columna completament observada per construir el clustering.")
  }
  
  dissimMatrix <- daisy(data[, colsNoMiss, drop = FALSE], metric = "gower", stand = TRUE)
  distMatrix <- dissimMatrix^2
  
  hcdata <- hclust(distMatrix, method = "ward.D2")

  plot(hcdata, main = "Dendrograma MIMMI", 
       labels = FALSE, hang = -1, xlab = "Individus (Allotjaments)", sub = "")
  
  # Si vols afegir caixes de colors per als clústers (com als apunts):
  rect.hclust(hcdata, k = priork, border = "red")
  
  nk <- priork
  partition <- cutree(hcdata, nk)
  
  CompleteData <- data
  newCol <- K + 1
  CompleteData[, newCol] <- partition
  names(CompleteData)[newCol] <- "ClassAux"
  
  setOfClasses <- sort(unique(partition))
  imputationTable <- data.frame(row.names = paste0("c", setOfClasses))
  p <- 1
  
  for (k in colsMiss) {
    rowsWithFullValues <- !is.na(CompleteData[, k])
    
    if (is.numeric(CompleteData[, k])) {
      imputingValues <- aggregate(
        CompleteData[rowsWithFullValues, k],
        by = list(partition[rowsWithFullValues]),
        FUN = mean
      )
    } else {
      imputingValues <- aggregate(
        CompleteData[rowsWithFullValues, k],
        by = list(partition[rowsWithFullValues]),
        FUN = Mode_MIMMI
      )
    }
    
    for (c in setOfClasses) {
      valor_c <- imputingValues[imputingValues[, 1] == c, 2]
      if (length(valor_c) == 1) {
        CompleteData[is.na(CompleteData[, k]) & partition == c, k] <- valor_c
      }
    }
    
    valors_ordenats <- imputingValues[match(setOfClasses, imputingValues[, 1]), 2]
    imputationTable[, p] <- valors_ordenats
    names(imputationTable)[p] <- names(data)[k]
    p <- p + 1
  }
  
  out <- new.env()
  out$imputedData <- CompleteData
  out$imputation <- imputationTable
  
  return(out)
}

dd_strategy3_mimmi <- dd_base

df_mimmi <- prepara_per_model(dd_base, treure_id = TRUE)

nzv_mimmi <- nearZeroVar(df_mimmi)
if (length(nzv_mimmi) > 0) {
  cat("MIMMI: s'eliminen", length(nzv_mimmi), "variables quasi constants per estabilitat.\n")
  df_mimmi <- df_mimmi[, -nzv_mimmi, drop = FALSE]
}

mimmi_result <- tryCatch({
  set.seed(123)
  MiMMi(df_mimmi, priork = 5)
}, error = function(e) {
  cat("\nMIMMI ha fallat.\n")
  cat("Missatge d'error:", conditionMessage(e), "\n")
  return(NULL)
})

if (!is.null(mimmi_result)) {
  dd_strategy3_mimmi_core <- mimmi_result$imputedData
  
  if ("ClassAux" %in% names(dd_strategy3_mimmi_core)) {
    dd_strategy3_mimmi_core <- dd_strategy3_mimmi_core %>% select(-ClassAux)
  }
  
  cols_mimmi <- intersect(names(dd_strategy3_mimmi_core), names(dd_strategy3_mimmi))
  dd_strategy3_mimmi[, cols_mimmi] <- dd_strategy3_mimmi_core[, cols_mimmi]
}

cat("\n--- ESTRATÈGIA 3C: MIMMI ---\n")
cat("Missings restants després de MIMMI:", sum(is.na(dd_strategy3_mimmi)), "\n")

if (!is.null(mimmi_result) && !is.null(mimmi_result$imputation)) {
  cat("\n--- TAULA DE VALORS D'IMPUTACIÓ MIMMI ---\n")
  print(mimmi_result$imputation)
}


# ==============================================================================
# 16. ESTRATÈGIA 3D: MISSFOREST
# ==============================================================================
# missForest = imputació basada en Random Forests

dd_strategy3_mf <- dd_base

df_mf <- prepara_per_model(dd_base, treure_id = TRUE)

imp_mf <- tryCatch({
  set.seed(123)
  missForest(df_mf, variablewise = TRUE, verbose = TRUE)
}, error = function(e) {
  cat("\nmissForest ha fallat.\n")
  cat("Missatge d'error:", conditionMessage(e), "\n")
  return(NULL)
})

if (!is.null(imp_mf)) {
  cols_mf <- intersect(names(imp_mf$ximp), names(dd_strategy3_mf))
  dd_strategy3_mf[, cols_mf] <- imp_mf$ximp[, cols_mf]
}

cat("\n--- ESTRATÈGIA 3D: MISSFOREST ---\n")
cat("Missings restants després de missForest:", sum(is.na(dd_strategy3_mf)), "\n")

if (!is.null(imp_mf)) {
  cat("OOB error missForest:\n")
  print(imp_mf$OOBerror)
}


# ==============================================================================
# 17. COMPARACIÓ BEFORE VS AFTER (AMB TEST KOLMOGOROV-SMIRNOV)
# ==============================================================================
library(dplyr)

vars_numeric_compare <- c(
  "price", "beds", "bathrooms", "bedrooms",
  "host_response_rate", "host_acceptance_rate"
)

# Funció auxiliar per calcular els estadístics bàsics
resum_numeric <- function(x) {
  c(
    n = sum(!is.na(x)),
    n_miss = sum(is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

# Funció per calcular l'estadístic K-S (Distància 'D') comparant amb l'original
# Com més proper a 0, millor és la imputació (més s'assembla a l'original).
calcula_ks <- function(original, imputat) {
  orig_net <- na.omit(original)
  imp_net <- na.omit(imputat)
  
  if(length(orig_net) < 2 || length(imp_net) < 2) return(NA)
  
  # Fem el test i n'extraiem només l'estadístic D
  test <- suppressWarnings(ks.test(orig_net, imp_net))
  return(test$statistic)
}

# Construcció de la taula ampliada
comparacio_estadistics <- lapply(vars_numeric_compare, function(v) {
  
  # 1. Calculem els descriptius bàsics com ja tenies
  df_base <- data.frame(
    variable = v,
    metrica = names(resum_numeric(dd_base[[v]])),
    original = as.numeric(resum_numeric(dd_base[[v]])),
    simple = as.numeric(resum_numeric(dd_strategy2_simple[[v]])),
    knn = as.numeric(resum_numeric(dd_strategy3_knn[[v]])),
    mice = as.numeric(resum_numeric(dd_strategy3_mice[[v]])),
    mimmi = as.numeric(resum_numeric(dd_strategy3_mimmi[[v]])),
    missForest = as.numeric(resum_numeric(dd_strategy3_mf[[v]]))
  )
  
  # 2. Afegim una fila extra per a cada variable amb la mètrica "KS_dist"
  fila_ks <- data.frame(
    variable = v,
    metrica = "KS_dist",
    original = 0, # L'original contra ell mateix té distància 0
    simple = calcula_ks(dd_base[[v]], dd_strategy2_simple[[v]]),
    knn = calcula_ks(dd_base[[v]], dd_strategy3_knn[[v]]),
    mice = calcula_ks(dd_base[[v]], dd_strategy3_mice[[v]]),
    mimmi = calcula_ks(dd_base[[v]], dd_strategy3_mimmi[[v]]),
    missForest = calcula_ks(dd_base[[v]], dd_strategy3_mf[[v]])
  )
  
  bind_rows(df_base, fila_ks)
})

comparacio_estadistics <- bind_rows(comparacio_estadistics)

cat("\n--- TAULA D'ESTADÍSTICS COMPARATIUS (AMB TEST K-S) ---\n")
print(comparacio_estadistics)


# ------------------------------------------------------------------------------
# 17.2 Gràfics de densitat per variables numèriques
# ------------------------------------------------------------------------------
plot_density_safe <- function(x, ...) {
  x <- na.omit(x)
  if (length(x) < 2 || length(unique(x)) < 2) return(FALSE)
  lines(density(x), ...)
  return(TRUE)
}

for (v in vars_numeric_compare) {
  
  x0 <- na.omit(dd_base[[v]])
  
  if (length(x0) < 2 || length(unique(x0)) < 2) {
    cat("No es pot fer density per", v, "\n")
    next
  }
  
  plot(
    density(x0),
    main = paste("Comparació de distribució per", v),
    xlab = v,
    lwd = 2
  )
  
  plot_density_safe(dd_strategy2_simple[[v]], col = "blue", lwd = 2)
  plot_density_safe(dd_strategy3_knn[[v]], col = "red", lwd = 2)
  plot_density_safe(dd_strategy3_mice[[v]], col = "darkgreen", lwd = 2)
  plot_density_safe(dd_strategy3_mimmi[[v]], col = "orange", lwd = 2)
  plot_density_safe(dd_strategy3_mf[[v]], col = "purple", lwd = 2)
  
  legend(
    "topright",
    legend = c("Original", "Simple", "KNN", "MICE", "MIMMI", "missForest"),
    col = c("black", "blue", "red", "darkgreen", "orange", "purple"),
    lty = 1,
    lwd = 2,
    cex = 0.8
  )
}


# ------------------------------------------------------------------------------
# 17.3 Comparació de relacions bivariants
# ------------------------------------------------------------------------------
vars_bivariants <- c("bathrooms", "beds", "bedrooms")

for (v in vars_bivariants) {
  
  print(
    ggplot(dd_base, aes(x = .data[[v]], y = price)) +
      geom_point(alpha = 0.15) +
      labs(title = paste("Original:", v, "vs price")) +
      theme_minimal()
  )
  
  print(
    ggplot(dd_strategy2_simple, aes(x = .data[[v]], y = price)) +
      geom_point(alpha = 0.15, color = "blue") +
      labs(title = paste("Simple:", v, "vs price")) +
      theme_minimal()
  )
  
  print(
    ggplot(dd_strategy3_knn, aes(x = .data[[v]], y = price)) +
      geom_point(alpha = 0.15, color = "red") +
      labs(title = paste("KNN:", v, "vs price")) +
      theme_minimal()
  )
  
  print(
    ggplot(dd_strategy3_mice, aes(x = .data[[v]], y = price)) +
      geom_point(alpha = 0.15, color = "darkgreen") +
      labs(title = paste("MICE:", v, "vs price")) +
      theme_minimal()
  )
  
  print(
    ggplot(dd_strategy3_mimmi, aes(x = .data[[v]], y = price)) +
      geom_point(alpha = 0.15, color = "orange") +
      labs(title = paste("MIMMI:", v, "vs price")) +
      theme_minimal()
  )
  
  print(
    ggplot(dd_strategy3_mf, aes(x = .data[[v]], y = price)) +
      geom_point(alpha = 0.15, color = "purple") +
      labs(title = paste("missForest:", v, "vs price")) +
      theme_minimal()
  )
}


# ==============================================================================
# 18. COMPARACIÓ PER VARIABLES CATEGÒRIQUES
# ==============================================================================
vars_cat_compare <- c(
  "host_is_superhost", "instant_bookable", "room_type",
  "property_type", "bath_is_shared", "listing_is_new"
)

prop_named_safe <- function(x, lvls) {
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- "NA_missing"
  
  tt <- prop.table(table(x_chr))
  
  out <- setNames(rep(0, length(lvls)), lvls)
  out[names(tt)] <- as.numeric(tt)
  
  return(out)
}

comparacio_categoriques <- lapply(vars_cat_compare, function(v) {
  
  get_levels <- function(x) {
    x_chr <- as.character(x)
    x_chr[is.na(x_chr)] <- "NA_missing"
    sort(unique(x_chr))
  }
  
  nivells <- sort(unique(c(
    get_levels(dd_base[[v]]),
    get_levels(dd_strategy2_simple[[v]]),
    get_levels(dd_strategy3_knn[[v]]),
    get_levels(dd_strategy3_mice[[v]]),
    get_levels(dd_strategy3_mimmi[[v]]),
    get_levels(dd_strategy3_mf[[v]])
  )))
  
  original_vec   <- prop_named_safe(dd_base[[v]], nivells)
  simple_vec     <- prop_named_safe(dd_strategy2_simple[[v]], nivells)
  knn_vec        <- prop_named_safe(dd_strategy3_knn[[v]], nivells)
  mice_vec       <- prop_named_safe(dd_strategy3_mice[[v]], nivells)
  mimmi_vec      <- prop_named_safe(dd_strategy3_mimmi[[v]], nivells)
  missforest_vec <- prop_named_safe(dd_strategy3_mf[[v]], nivells)
  
  data.frame(
    variable = rep(v, length(nivells)),
    nivell = nivells,
    original = as.numeric(original_vec),
    simple = as.numeric(simple_vec),
    knn = as.numeric(knn_vec),
    mice = as.numeric(mice_vec),
    mimmi = as.numeric(mimmi_vec),
    missForest = as.numeric(missforest_vec),
    row.names = NULL
  )
})

comparacio_categoriques <- bind_rows(comparacio_categoriques)

cat("\n--- TAULA DE PROPORCIONS CATEGÒRIQUES ---\n")
print(comparacio_categoriques)


# ==============================================================================
# 19. RESUM AUTOMÀTIC DE LA PÈRDUA / IMPUTACIÓ
# ==============================================================================
resum_estrategies <- data.frame(
  estrategia = c(
    "Base original",
    "E1 Eliminació global",
    "E2 Imputació simple",
    "E3A KNN",
    "E3B MICE",
    "E3C MIMMI",
    "E3D missForest"
  ),
  n_files = c(
    nrow(dd_base),
    nrow(dd_strategy1_global),
    nrow(dd_strategy2_simple),
    nrow(dd_strategy3_knn),
    nrow(dd_strategy3_mice),
    nrow(dd_strategy3_mimmi),
    nrow(dd_strategy3_mf)
  ),
  n_columnes = c(
    ncol(dd_base),
    ncol(dd_strategy1_global),
    ncol(dd_strategy2_simple),
    ncol(dd_strategy3_knn),
    ncol(dd_strategy3_mice),
    ncol(dd_strategy3_mimmi),
    ncol(dd_strategy3_mf)
  ),
  missings_restants = c(
    sum(is.na(dd_base)),
    sum(is.na(dd_strategy1_global)),
    sum(is.na(dd_strategy2_simple)),
    sum(is.na(dd_strategy3_knn)),
    sum(is.na(dd_strategy3_mice)),
    sum(is.na(dd_strategy3_mimmi)),
    sum(is.na(dd_strategy3_mf))
  )
)

cat("\n--- RESUM GLOBAL D'ESTRATÈGIES ---\n")
print(resum_estrategies)


# ==============================================================================
# 20. DECISIÓ FINAL (GUIA INTERPRETATIVA)
# ==============================================================================
# Recomanació d'interpretació:
#   - Si una variable sembla MCAR i la pèrdua és petita, eliminació pot ser acceptable.
#   - Si la variable és numèrica i simple/mediana conserva bé la distribució, es pot defensar.
#   - Si el mecanisme sembla MAR, KNN / MICE / MIMMI / missForest solen tenir més sentit.
#   - Si sospites MNAR, cap imputació és del tot "neta": cal justificar prudència.
#
# IMPORTANT SOBRE MICE:
#   - Aquí MICE no s'aplica a totes les variables perquè, en fer-ho, apareixia
#     singularitat computacional.
#   - Per això s'ha fet servir un únic subconjunt estable i justificat.
#   - Les variables excloses es mostren a consola a l'inici del bloc MICE.

# Posem a 0 els missings de les variables de review, no imputem, ja que sabem que 
# els valors faltants son deguts a listing_is_new

dd_final_imputat <- dd_base

# 2. ELIMINACIÓ DE REDUNDÀNCIES ESTRUCTURALS
# Eliminem 'host_response_time' perquè és redundant amb 'host_response_rate' (numèrica)
# i així evitem soroll i multicol·linearitat.
dd_final_imputat <- dd_final_imputat %>% select(-host_response_time)

# 2. TRACTAMENT ESTRUCTURAL DE LES RESSENYES (Missing Indicator Method)
# Aquestes variables tenen NAs estructurals (pisos nous sense estrenar).
# Com que ja tenim la variable 'listing_is_new', omplim els NAs amb un -1 tècnic.
vars_reviews_num <- c(
  "review_scores_rating", "review_scores_accuracy", 
  "review_scores_cleanliness", "review_scores_checkin", 
  "review_scores_communication", "review_scores_location", 
  "review_scores_value", "reviews_per_month"
)

# Substituïm els NAs per -1 només en aquestes columnes
for (v in vars_reviews_num) {
  dd_final_imputat[[v]][is.na(dd_final_imputat[[v]])] <- -1
}

# NOTA PER A LA MEMÒRIA: Les variables de data 'first_review' i 'last_review' 
# es deixen intencionadament amb els seus NAs estructurals. 
# Es tractaran posteriorment en la fase de Feature Engineering 
# (convertint-les a dies transcorreguts).

# 4. IMPUTACIÓ AMB MICE (Guanyadors segons test K-S i Gràfics de Densitat)
# MICE ha demostrat ser el millor preservant la distribució de preu i llits.
dd_final_imputat$price <- dd_strategy3_mice$price
dd_final_imputat$beds  <- dd_strategy3_mice$beds

# 5. IMPUTACIÓ AMB KNN (Guanyador en robustesa i variables amb pocs missings)
# Utilitzem KNN per a la resta de variables numèriques i totes les categòriques.
vars_from_knn <- c(
  "bathrooms", "bedrooms", "host_response_rate", 
  "host_acceptance_rate", "estimated_revenue_l365d",
  "host_is_superhost", "host_has_profile_pic", 
  "host_identity_verified", "bath_is_shared"
)

for (v in vars_from_knn) {
  if (v %in% names(dd_strategy3_knn)) {
    dd_final_imputat[[v]] <- dd_strategy3_knn[[v]]
  }
}

# 6. COMPROVACIÓ FINAL DE MISSINGS
# Excloem les dates (first_review, last_review) que es tractaran en Feature Engineering
missings_finals <- sum(is.na(dd_final_imputat %>% select(-any_of(c("first_review", "last_review")))))

cat("\nResum del dataset final:")
cat("\n- Variables eliminades per redundància: host_response_time")
cat("\n- Missings restants (excloent dates):", missings_finals, "\n")

dd_final_reunificat <- dd_final_imputat %>%
  left_join(dd_text, by = "id")

# 3. ELIMINACIÓ DE COLUMNES TEMPORALS O AUXILIARS
# Eliminem la variable 'n_miss_individu' si encara volta per aquí, 
# ja que només era per a l'anàlisi de patrons.
if ("n_miss_individu" %in% names(dd_final_reunificat)) {
  dd_final_reunificat <- dd_final_reunificat %>% select(-n_miss_individu)
}

# 4. COMPROVACIÓ DE DIMENSIONS
cat("Dimensions finals del dataset:", nrow(dd_final_reunificat), "files x", ncol(dd_final_reunificat), "columnes.\n")

# 5. COMPROVACIÓ FINAL DE MISSINGS (SENSE COMPTAR DATES NI TEXT)
# Nota: Les variables de text poden tenir missings (descripcions buides), la qual cosa és normal.
missings_tecnics <- sum(is.na(dd_final_reunificat %>% 
                                select(-any_of(c("first_review", "last_review"))) %>%
                                select(-all_of(vars_text))))

cat("- Missings en variables numèriques/categòriques (excloent dates):", missings_tecnics, "\n")


saveRDS(dd_final_reunificat, "../data/dataset_imputat.rds")