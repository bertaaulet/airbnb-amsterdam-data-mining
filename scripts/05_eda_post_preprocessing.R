# ==============================================================================
# EDA POST-PREPROCESSAMENT
# ==============================================================================

# Càrrega de llibreries ========================================================
library(tidyverse)
library(visdat)
library(inspectdf)
library(skimr)
library(DataExplorer)
library(SmartEDA)
library(ggcorrplot)
library(patchwork)

# ==============================================================================
# 0. CÀRREGA DE DADES
# ==============================================================================
dades_pre <- readRDS("../data/dataset_feature_selection_final.rds")

cat("=== DIMENSIONS POST-PREPROCESSAMENT ===\n")
cat("Files:", nrow(dades_pre), " | Columnes:", ncol(dades_pre), "\n")


# ==============================================================================
# 1. VARIABLES A EXCLOURE DE L'ANÀLISI (identificadors i text lliure)
# ==============================================================================
vars_excloure <- c(
  "id", "listing_url", "picture_url", "host_id", "host_url",
  "host_thumbnail_url", "host_picture_url", "host_verifications",
  "amenities", "license", "name", "description",
  "neighborhood_overview", "host_about", "host_name",
  "host_location"
)

dades_analisi <- dades_pre %>% select(-any_of(vars_excloure))

cat("Variables per analitzar:", ncol(dades_analisi), "\n")


# ==============================================================================
# 2. VERIFICACIÓ DE QUALITAT POST-PREPROCESSAMENT
# ==============================================================================

cat("\n=== 2.1 RESUM GLOBAL (introduce) ===\n")
introduce(dades_analisi)
plot_intro(dades_analisi) # hauria de mostrar 0% missings

cat("\n=== 2.2 RESUM ESTADÍSTIC COMPLET (skim) ===\n")
skim(dades_analisi)


# ==============================================================================
# 3. VERIFICACIÓ DE CONSISTÈNCIA POST-PREPROCESSAMENT
# ==============================================================================

cat("\n=== 4. ERRORS DE RANG (post-preprocessament) ===\n")

errors_rang_post <- dades_analisi %>%
  summarise(
    # Valors econòmicament/lògicament impossibles
    `Preu <= 0`                     = sum(price <= 0,                        na.rm = TRUE),
    `Capacitat <= 0`                = sum(accommodates <= 0,                 na.rm = TRUE),
    
    # Inconsistència lògica entre variables
    `Nits min > Nits max`           = sum(minimum_nights > maximum_nights,   na.rm = TRUE),
    
    # Valors fora del domini físic possible
    `Disponibilitat > 365 dies`     = sum(availability_365 > 365,            na.rm = TRUE),
    `Ocupació estimada > 365 dies`  = sum(estimated_occupancy_l365d > 365,   na.rm = TRUE),
    
    # Variables construïdes: no poden ser negatives per construcció
    `beds_per_bedroom < 0`          = sum(beds_per_bedroom < 0,              na.rm = TRUE),
    `distancia_centre_km < 0`       = sum(distancia_centre_km < 0,           na.rm = TRUE),
    `amenities_score < 0`           = sum(amenities_score < 0,
                                          na.rm = TRUE),
    # Cap variable log_* pot ser negativa (log1p(max(x,0)) >= 0 sempre)
    !!!setNames(
      lapply(names(dades_analisi)[startsWith(names(dades_analisi), "log_")],
             function(v) sum(dades_analisi[[v]] < 0, na.rm = TRUE)),
      paste0(names(dades_analisi)[startsWith(names(dades_analisi), "log_")], " < 0")
    )) %>%
  pivot_longer(everything(),
               names_to  = "Tipus_Inconsistencia",
               values_to = "Frequencia") %>%
  filter(Frequencia > 0)

if (nrow(errors_rang_post) == 0) {
  cat("Cap inconsistència de rang detectada.\n")
} else {
  cat("Inconsistències residuals:\n")
  print(errors_rang_post)
}


# ==============================================================================
# 4. AED UNIVARIANT POST-PREPROCESSAMENT
# ==============================================================================

# --- 4.1 Variables Numèriques originals (no logarítmiques) --------------------
vars_num_orig <- c("price", "accommodates", "bathrooms", "bedrooms", "beds",
                   "minimum_nights", "maximum_nights", "availability_365",
                   "number_of_reviews", "estimated_occupancy_l365d",
                   "estimated_revenue_l365d", "reviews_per_month",
                   "amenities_score", "beds_per_bedroom", "distancia_centre_km",
                   "dies_antiguitat_listing", "dies_recencia_review",
                   "dies_antiguitat_host")

# Estadístics descriptius (SmartEDA)
taula_num_post <- ExpNumStat(
  dades_analisi %>% select(all_of(intersect(vars_num_orig, names(dades_analisi)))),
  by = "A", gp = NULL,
  Qnt = seq(0, 1, 0.1),
  MesofShape = 2, Outlier = TRUE, round = 2
)
print(taula_num_post)

# Histogrames (DataExplorer)
vars_valid <- intersect(vars_num_orig, names(dades_analisi))
for (i in seq(1, length(vars_valid), by = 9)) {
  print(plot_histogram(
    dades_analisi %>% select(all_of(vars_valid[i:min(i + 8, length(vars_valid))])),
    nrow = 3, ncol = 3
  ))
}

# --- 4.2 Variables Numèriques logarítmiques (noves) ---------------------------
vars_log <- names(dades_analisi)[startsWith(names(dades_analisi), "log_")]

cat("\n=== Variables logarítmiques creades:", paste(vars_log, collapse = ", "), "===\n")

plot_histogram(dades_analisi %>% select(all_of(vars_log)),
               title = "Distribució de variables log-transformades")

# Comparativa distribució: original vs log (per a price)
p_orig <- ggplot(dades_analisi, aes(x = price)) +
  geom_histogram(bins = 60, fill = "#E15759", alpha = 0.8) +
  labs(title = "Price (original)", x = "Price (€)", y = "Freqüència") +
  theme_minimal()

p_log <- ggplot(dades_analisi, aes(x = log_price)) +
  geom_histogram(bins = 60, fill = "#59A14F", alpha = 0.8) +
  labs(title = "log_price (log1p transformació)", x = "log(Price + 1)", y = "Freqüència") +
  theme_minimal()

print(p_orig + p_log)

# --- 4.3 Noves variables numèriques derivades (Feature Engineering) -----------

# Boxplots per a les noves variables
noves_vars_num <- c("amenities_score", "beds_per_bedroom", "distancia_centre_km",
                    "dies_antiguitat_listing", "dies_recencia_review", "dies_antiguitat_host")

plot_list <- list()
for (var in intersect(noves_vars_num, names(dades_analisi))) {
  plot_list <- c(plot_list, list(
    ggplot(dades_analisi, aes(y = .data[[var]])) +
      geom_boxplot(fill = "#4E79A7", alpha = 0.7,
                   outlier.colour = "red", outlier.size = 0.8, na.rm = TRUE) +
      geom_jitter(aes(x = 0), width = 0.05, alpha = 0.05,
                  colour = "#4E79A7", size = 0.3) +
      labs(title = paste("Distribució de", var), y = var, x = NULL) +
      theme_minimal()
  ))
}
print(wrap_plots(plot_list, ncol = 3))

# --- 4.4 Variables Categòriques (originals + noves) ---------------------------
vars_cat <- dades_analisi %>%
  select(where(is.factor), where(is.logical)) %>%
  names()

cat("\n=== Variables categòriques:", paste(vars_cat, collapse = ", "), "===\n")

# Freqüències i percentatges (SmartEDA)
ExpCatStat(
  dades_analisi %>% select(all_of(vars_cat)),
  Target  = "room_type",   # variable target qualsevol per obtenir les freqüències
  result  = "Stat",
  clim    = 30
)

# Gràfics de barres (DataExplorer)
for (i in seq(1, length(vars_cat), by = 6)) {
  plot_bar(
    dades_analisi %>% select(all_of(vars_cat[i:min(i + 5, length(vars_cat))])),
    nrow = 2, ncol = 3
  )
}

# Noves variables categòriques del feature engineering
noves_vars_cat <- c("host_type", "cat_acceptance_rate",
                    "cat_response_rate", "cat_review_scores_rating",
                    "listing_is_new", "bath_is_shared")

plot_list <- list()
for (var in intersect(noves_vars_cat, names(dades_analisi))) {
  freq_df <- dades_analisi %>%
    count(.data[[var]]) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    arrange(desc(n))
  
  plot_list <- c(plot_list, list(
    ggplot(freq_df, aes(x = reorder(.data[[var]], n), y = n, fill = .data[[var]])) +
      geom_col(show.legend = FALSE, alpha = 0.85) +
      geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.2) +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(title = paste("Distribució de", var),
           x = var, y = "Freqüència") +
      expand_limits(y = max(freq_df$n) * 1.15) +
      theme_minimal()
  ))
}
print(wrap_plots(plot_list, ncol = 3))


# ==============================================================================
# 5. AED BIVARIANTE POST-PREPROCESSAMENT
# ==============================================================================

# --- 5.1 Matriu de correlació (variables numèriques clau) ---------------------

# Usem les versions log per a la matriu de correlació (menys distorsió)
vars_corr <- c("log_price", "accommodates", "bathrooms", "bedrooms", "beds",
               "log_minimum_nights", "availability_365", "log_number_of_reviews",
               "log_estimated_revenue_l365d", "log_estimated_occupancy_l365d",
               "amenities_score", "distancia_centre_km",
               "beds_per_bedroom", "log_dies_antiguitat_listing",
               "log_reviews_per_month")

vars_corr_ok <- intersect(vars_corr, names(dades_analisi))

corr_matrix <- dades_analisi %>%
  select(all_of(vars_corr_ok)) %>%
  cor(use = "pairwise.complete.obs", method = "spearman")

ggcorrplot(corr_matrix,
           method    = "square",
           type      = "lower",
           lab       = TRUE,
           lab_size  = 2.5,
           tl.cex    = 8,
           colors    = c("#E15759", "white", "#59A14F"),
           title     = "Matriu de Correlació (post-preprocessament)",
           ggtheme   = theme_minimal())

# Correlació amb log_price ordenada
corr_price <- corr_matrix["log_price", ] %>%
  sort(decreasing = TRUE) %>%
  enframe(name = "Variable", value = "Correlacio_Spearman") %>%
  filter(Variable != "log_price")

print(corr_price, n = Inf)

ggplot(corr_price, aes(x = reorder(Variable, Correlacio_Spearman),
                       y = Correlacio_Spearman,
                       fill = Correlacio_Spearman > 0)) +
  geom_col(alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#59A14F", "FALSE" = "#E15759"),
                    labels  = c("Positiva", "Negativa")) +
  labs(title = "Correlació de Spearman amb log_price",
       x = NULL, y = "Correlació", fill = "Direcció") +
  theme_minimal()

# --- 5.2 Numèrica vs Categòrica (log_price per grups clau) -------------------

plot_boxplot_grup <- function(data, x_var, y_var, top_n = NULL) {
  df <- data %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
  if (!is.null(top_n)) {
    nivells <- df %>%
      count(.data[[x_var]], sort = TRUE) %>%
      slice_head(n = top_n) %>%
      pull(.data[[x_var]])
    df <- df %>%
      filter(.data[[x_var]] %in% nivells) %>%
      mutate(across(all_of(x_var), ~ droplevels(factor(.))))
  }
  ggplot(df, aes(x = reorder(.data[[x_var]], .data[[y_var]],
                              FUN = median, na.rm = TRUE),
                 y = .data[[y_var]])) +
    geom_boxplot(fill = "#4E79A7", alpha = 0.6,
                 outlier.colour = "red", outlier.size = 0.6, na.rm = TRUE) +
    stat_summary(fun = median, geom = "point",
                 shape = 18, size = 2.5, colour = "white") +
    coord_flip() +
    labs(title = paste(y_var, "per", x_var), x = x_var, y = y_var) +
    theme_minimal()
}

# Parelles rellevants
parelles <- list(
  list("room_type",              "log_price", NULL),
  list("neighbourhood_cleansed", "log_price", 10),
  list("host_is_superhost",      "log_price", NULL),
  list("instant_bookable",       "log_price", NULL),
  list("host_type",              "log_price", NULL),
  list("listing_is_new",         "log_price", NULL),
  list("cat_acceptance_rate",    "log_price", NULL),
  list("cat_response_rate",      "log_price", NULL),
  list("bath_is_shared",         "log_price", NULL)
)

plot_list <- list()
for (i in seq_along(parelles)) {
  plot_list <- c(plot_list, list(
    plot_boxplot_grup(dades_analisi, parelles[[i]][[1]],
                      parelles[[i]][[2]],
                      parelles[[i]][[3]])
  ))
  if (i %% 3 == 0 || i == length(parelles)) {
    print(wrap_plots(plot_list, ncol = 1))
    plot_list <- list()
  }
}

# log_price vs distancia al centre (scatter)
ggplot(dades_analisi, aes(x = distancia_centre_km, y = log_price)) +
  geom_point(alpha = 0.2, size = 0.7, colour = "#4E79A7") +
  geom_smooth(method = "loess", se = TRUE, colour = "red", linewidth = 0.9) +
  labs(title  = "log_price vs Distància al Centre d'Amsterdam",
       x = "Distància al centre (km)", y = "log(Price + 1)") +
  theme_minimal()

# amenities_score vs log_price
ggplot(dades_analisi, aes(x = amenities_score, y = log_price)) +
  geom_point(alpha = 0.2, size = 0.7, colour = "#59A14F") +
  geom_smooth(method = "lm", se = TRUE, colour = "red", linewidth = 0.9) +
  labs(title = "log_price vs Puntuació d'Amenities",
       x = "amenities_score", y = "log(Price + 1)") +
  theme_minimal()

# --- 5.3 Qualitativa vs Qualitativa (amb tests Chi²) -------------------------

taula_chi2 <- function(data, var1, var2) {
  cat("\n==============================\n")
  cat(paste("Contingència:", var1, "×", var2), "\n")
  cat("==============================\n")
  t <- table(data[[var1]], data[[var2]], useNA = "no")
  print(round(prop.table(t, margin = 1) * 100, 1))
  test <- chisq.test(t, simulate.p.value = TRUE)
  cat(paste0("Chi² = ", round(test$statistic, 2),
             "  |  p-valor = ", format.pval(test$p.value, digits = 3),
             "  |  B = 2000 permutacions\n"))
}

taula_chi2(dades_analisi, "room_type",         "host_is_superhost")
taula_chi2(dades_analisi, "room_type",         "instant_bookable")
taula_chi2(dades_analisi, "host_is_superhost", "instant_bookable")
taula_chi2(dades_analisi, "host_type",         "room_type")
taula_chi2(dades_analisi, "host_type",         "host_is_superhost")
taula_chi2(dades_analisi, "listing_is_new",    "bath_is_shared")
taula_chi2(dades_analisi, "cat_acceptance_rate", "cat_response_rate")

# Stacked bar: host_type × room_type
dades_analisi %>%
  filter(!is.na(host_type), !is.na(room_type)) %>%
  ggplot(aes(x = host_type, fill = room_type)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Tipus d'amfitrió per Room Type",
       x = "Tipus d'amfitrió", y = "Proporció", fill = "Room Type") +
  theme_minimal()

# Stacked bar: cat_acceptance_rate × cat_response_rate
dades_analisi %>%
  filter(!is.na(cat_acceptance_rate), !is.na(cat_response_rate)) %>%
  ggplot(aes(x = cat_acceptance_rate, fill = cat_response_rate)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Taxa d'Acceptació × Taxa de Resposta",
       x = "cat_acceptance_rate", y = "Proporció",
       fill = "cat_response_rate") +
  theme_minimal()


# ==============================================================================
# 6. ANÀLISI GEOESPACIAL (latitud / longitud)
# ==============================================================================

# Mapa de calor de preus per coordenades
ggplot(dades_analisi, aes(x = longitude, y = latitude, colour = log_price)) +
  geom_point(alpha = 0.4, size = 0.6) +
  scale_colour_gradient2(low  = "#59A14F", mid = "#FFBE7D",
                         high = "#E15759", midpoint = median(dades_analisi$log_price)) +
  labs(title  = "Distribució Geogràfica de log_price – Amsterdam",
       x = "Longitud", y = "Latitud", colour = "log(Preu)") +
  coord_fixed(ratio = 1.3) +
  theme_minimal()

# Mapa: distribució de room_type per coordenades
ggplot(dades_analisi, aes(x = longitude, y = latitude, colour = room_type)) +
  geom_point(alpha = 0.4, size = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Distribució Geogràfica per Room Type – Amsterdam",
       x = "Longitud", y = "Latitud", colour = "Room Type") +
  coord_fixed(ratio = 1.3) +
  theme_minimal()

# Mapa: host_type (Professional vs Particular)
ggplot(dades_analisi, aes(x = longitude, y = latitude, colour = host_type)) +
  geom_point(alpha = 0.35, size = 0.5) +
  scale_colour_manual(values = c("Professional" = "#E15759",
                                 "Particular"   = "#4E79A7")) +
  labs(title = "Distribució Geogràfica per Tipus d'Amfitrió",
       x = "Longitud", y = "Latitud", colour = "Tipus Amfitrió") +
  coord_fixed(ratio = 1.3) +
  theme_minimal()
