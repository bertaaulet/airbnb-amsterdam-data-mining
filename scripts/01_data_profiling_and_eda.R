# ==============================================================================
# FASE 1: Data Profiling i Exploratory Data Analysis (EDA)
# Projecte: Airbnb Amsterdam
# ==============================================================================

# Càrrega de llibreries ========================================================
library(tidyverse)
library(visdat)
library(inspectdf)
library(skimr)
library(DataExplorer)
library(SmartEDA)
library(dataReporter)
library(patchwork)

# Càrrega i primera inspecció de les dades =====================================
# Les dades es llegeixen des de la carpeta principal 'data/'
dades <- read.csv("../data/listings.csv", stringsAsFactors = FALSE)
dim(dades)
glimpse(dades)

# ==============================================================================
# 1. Perfilat de la Qualitat de les Dades (Data Profiling)
# ==============================================================================

# 1.1 Conversió d'espais buits i "N/A" a valors NA reals -----------------------
dades <- dades %>%
  mutate(across(where(is.character), ~ na_if(trimws(.), ""))) %>%
  mutate(across(where(is.character), ~ na_if(., "N/A")))

# 1.2 Correcció de tipus -------------------------------------------------------
dades <- dades %>%
  mutate(
    # Neteja numèrica (preus i percentatges)
    price                = as.numeric(gsub("[$,]", "", price)),
    host_response_rate   = as.numeric(gsub("%", "", host_response_rate)) / 100,
    host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)) / 100,
    
    # Conversió de text "t"/"f" a variables lògiques (TRUE/FALSE)
    across(c(host_is_superhost, host_has_profile_pic, host_identity_verified,
             has_availability, instant_bookable), ~ . == "t"),
    
    # Conversió de dates
    across(c(last_scraped, host_since, calendar_last_scraped,
             first_review, last_review), as.Date),
    
    # Conversió de la resta de cadenes a factors
    across(where(is.character), as.factor)
  )

# 1.3 Visió general ------------------------------------------------------------

## DataExplorer: resum global de l'estructura
introduce(dades)
plot_intro(dades)

## SmartEDA: resum global i estructura detallada
ExpData(data = dades, type = 1)
ExpData(data = dades, type = 2)

## visdat: tipus de variables + distribució de NAs per columna i fila
vis_dat(dades)
vis_miss(dades)

## inspectdf: tipus i memòria
inspect_types(dades) %>% show_plot()
inspect_mem(dades)   %>% show_plot()

# 1.4 Resum estadístic global --------------------------------------------------
skim(dades)

# 1.5 Identificació de variables constants o 100% buides ----------------------
resum_constants <- dades %>%
  summarise(across(everything(), list(
    n_unics = ~ n_distinct(., na.rm = TRUE),
    n_na    = ~ sum(is.na(.))
  ))) %>%
  pivot_longer(
    everything(),
    names_to      = c("Variable", ".value"),
    names_pattern = "(.*)_(n_unics|n_na)"
  ) %>%
  filter(n_unics <= 1) %>%
  mutate(
    Tipus_Problema = case_when(
      n_na == nrow(dades) ~ "100% Buida (Tots NA)",
      n_unics == 1        ~ "Constant",
      TRUE                ~ "Altres"
    )
  ) %>%
  select(Variable, Tipus_Problema, Num_Missings = n_na) %>%
  arrange(Tipus_Problema, Variable)

print(resum_constants)

# Eliminació de les variables detectades
vars_constants <- resum_constants$Variable
dades <- dades %>% select(-all_of(vars_constants))

cat("Variables eliminades:", paste(vars_constants, collapse = ", "), "\n")
cat("Dimensions resultants:", nrow(dades), "files x", ncol(dades), "columnes\n")

# 1.6 Identificació i recompte de missings (> 0%) ------------------------------
missings_df <- dades %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to  = "Variable",
               values_to = "Num_Missings") %>%
  mutate(Percentatge = (Num_Missings / nrow(dades)) * 100) %>%
  filter(Num_Missings > 0) %>%
  arrange(desc(Percentatge))

print(missings_df, n = Inf)
plot_missing(dades)

# 1.7 Registres i IDs duplicats ------------------------------------------------
cat("\nFiles duplicades:", sum(duplicated(dades)), "\n")
cat("Allotjaments amb ID duplicat:", sum(duplicated(dades$id)), "\n")

# 1.8 Identificació de valors fora de rang i inconsistències -------------------
errors_rang <- dades %>%
  summarise(
    `Preu <= 0`                          = sum(price <= 0, na.rm = TRUE),
    `Capacitat <= 0`                     = sum(accommodates <= 0, na.rm = TRUE),
    `Nits mínimes > Nits màximes`        = sum(minimum_nights > maximum_nights,
                                               na.rm = TRUE),
    `Disp. 30d > 30`                     = sum(availability_30 > 30, na.rm = TRUE),
    `Disp. 60d > 60`                     = sum(availability_60 > 60, na.rm = TRUE),
    `Disp. 90d > 90`                     = sum(availability_90 > 90, na.rm = TRUE),
    `Disp. 365d > 365`                   = sum(availability_365 > 365, na.rm = TRUE),
    `Ocupació estimada > 365`            = sum(estimated_occupancy_l365d > 365,
                                               na.rm = TRUE),
    `Ocupació estimada = 255 (sospitós)` = sum(estimated_occupancy_l365d == 255,
                                               na.rm = TRUE),
    `Rating fora rang (0-5)`             = sum(review_scores_rating > 5 |
                                                 review_scores_rating < 0, na.rm = TRUE),
    `Taxa resposta fora rang (0-1)`      = sum(host_response_rate > 1 |
                                                 host_response_rate < 0, na.rm = TRUE),
    `maximum_nights sospitós (> 1125)`   = sum(maximum_nights > 1125, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(),
               names_to  = "Tipus_Inconsistencia",
               values_to = "Frequencia") %>%
  filter(Frequencia > 0) %>%
  arrange(desc(Frequencia))

print(errors_rang)

# 1.9 Report automàtic (dataReporter) ------------------------------------------
# makeDataReport(dades, output  = "html", file    = "profiling_report.Rmd", replace = TRUE)


# Guardem l'objecte R a la carpeta data per al següent script
saveRDS(dades, file = "../data/dades_profiling.rds")

# CSV
write.csv(dades, file = "../data/dades_profiling.csv", row.names = FALSE)

# ==============================================================================
# 2. AED Univariant
# ==============================================================================

# Variables identificadores i metadades a excloure de l'anàlisi descriptiva
vars_excloure <- c("id", "listing_url", "picture_url",
                   "host_id", "host_url", "host_thumbnail_url",
                   "host_picture_url", "amenities", "host_verifications",
                   "license", "name", "description", "neighborhood_overview",
                   "host_about", "neighbourhood")

dades_analisi <- dades %>% select(-all_of(vars_excloure))

# ==============================================================================
# 2.1 Variables Numèriques
# ==============================================================================

vars_num <- dades_analisi %>% select(where(is.numeric)) %>% names()

# --- ExpNumStat (SmartEDA) ----------------------------------------------------
taula_num <- ExpNumStat(
  dades_analisi,
  by         = "A",
  gp         = NULL,
  Qnt        = seq(0, 1, 0.1),
  MesofShape = 2,
  Outlier    = TRUE,
  round      = 2
)
print(taula_num[1:20, ])
print(taula_num[21:nrow(taula_num), ])

# --- inspect_num (inspectdf) --------------------------------------------------
for (i in seq(1, length(vars_num), by = 15)) {
  p <- dades_analisi[, vars_num[i:min(i+14, length(vars_num))]] %>%
    inspect_num() %>%
    show_plot()
  print(p)
}

# --- plot_histogram (DataExplorer) --------------------------------------------
plot_histogram(dades_analisi)

# --- Boxplots individuals per variables clau ----------------------------------
vars_clau_num <- c(
  # Econòmiques i de negoci
  "price", "estimated_revenue_l365d", "estimated_occupancy_l365d",
  # Capacitat de l'allotjament
  "accommodates", "bedrooms", "bathrooms",
  # Regles operatives
  "minimum_nights",
  # Perfil de l'amfitrió
  "host_response_rate", "host_acceptance_rate",
  # Activitat i reputació
  "reviews_per_month", "review_scores_rating"
)

plot_boxplot_var <- function(data, var) {
  ggplot(data, aes(y = .data[[var]])) +
    geom_boxplot(fill = "#4E79A7", alpha = 0.7,
                 outlier.colour = "red", outlier.size = 1,
                 na.rm = TRUE) +
    labs(title = paste("Boxplot:", var), y = var, x = "") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}

plot_list <- list()
for (i in seq_along(vars_clau_num)) {
  plot_list <- c(plot_list, list(plot_boxplot_var(dades_analisi, vars_clau_num[i])))  # <-- canvi
  if (i %% 6 == 0 || i == length(vars_clau_num)) {
    print(wrap_plots(plot_list, ncol = 3))
    plot_list <- list()
  }
}


# --- Histogrames i Boxplots de variables clau (Annex) ------------------------
plot_hist_var <- function(data, var) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(fill = "#4E79A7", alpha = 0.7, bins = 50, na.rm = TRUE) +
    labs(title = paste("Histograma:", var), x = var, y = "Freqüència") +
    theme_minimal()
}

# Tots els histogrames (6 en 6)
plot_list <- list()
for (i in seq_along(vars_clau_num)) {
  plot_list <- c(plot_list, list(plot_hist_var(dades_analisi, vars_clau_num[i])))
  if (i %% 6 == 0 || i == length(vars_clau_num)) {
    print(wrap_plots(plot_list, ncol = 3))
    plot_list <- list()
  }
}

# Tots els boxplots (6 en 6)
plot_list <- list()
for (i in seq_along(vars_clau_num)) {
  plot_list <- c(plot_list, list(plot_boxplot_var(dades_analisi, vars_clau_num[i])))
  if (i %% 6 == 0 || i == length(vars_clau_num)) {
    print(wrap_plots(plot_list, ncol = 3))
    plot_list <- list()
  }
}

# Histograma + Boxplot price
p_hist <- ggplot(dades_analisi, aes(x = price)) +
  geom_histogram(fill = "#4E79A7", alpha = 0.7, bins = 50, na.rm = TRUE) +
  labs(title = "Histograma: price", x = "price", y = "Freqüència") +
  theme_minimal()

p_hist + plot_boxplot_var(dades_analisi, "price")


# ==============================================================================
# 2.2 Variables Qualitatives
# ==============================================================================

# Conversió de lògiques a factor per compatibilitat amb inspectdf i SmartEDA
dades_cat <- dades_analisi %>%
  mutate(across(where(is.logical), ~ factor(
    ., levels = c(FALSE, TRUE), labels = c("No", "Sí")
  )))

# Variables qualitatives analitzables (factors, excloses dates i alta cardinalitat)
vars_qual_clau <- c("source", "room_type", "neighbourhood_cleansed",
                    "property_type", "host_response_time",
                    "host_is_superhost", "instant_bookable",
                    "host_identity_verified",
                    "host_has_profile_pic", "bathrooms_text")

# --- inspect_imb (inspectdf): desequilibri de categories ----------------------
inspect_imb(dades_cat) %>% show_plot()

# --- plot_bar (DataExplorer): barres per a variables rellevants ---------------
dades_cat %>%
  select(all_of(vars_qual_clau)) %>%
  plot_bar()

# --- ExpCTable (SmartEDA): taules de freqüència -------------------------------
ExpCTable(
  dades_cat %>% select(all_of(vars_qual_clau)),
  Target = NULL,
  margin = 1,
  clim   = 25,
  nlim   = 5,
  round  = 2,
  per    = TRUE
)

# --- Gràfics de barres ----------------------------------

plot_bar_var <- function(data, var, top_n = 15) {
  data %>%
    count(.data[[var]], sort = TRUE) %>%
    slice_head(n = top_n) %>%
    ggplot(aes(x = reorder(.data[[var]], n), y = n)) +
    geom_bar(stat = "identity", fill = "#4E79A7", alpha = 0.7) +
    coord_flip() +
    labs(title = var, x = "", y = "Freqüència") +
    theme_minimal()
}

plot_list <- list()
for (i in seq_along(vars_qual_clau)) {
  plot_list <- c(plot_list, list(plot_bar_var(dades_cat, vars_qual_clau[i])))
  if (i %% 6 == 0 || i == length(vars_qual_clau)) {
    print(wrap_plots(plot_list, ncol = 2))
    plot_list <- list()
  }
}

# --- Taules de freqüència detallades per variables clau ----------------------

cat("\n=== room_type ===\n")
dades_analisi %>%
  count(room_type, sort = TRUE) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  print()

cat("\n=== neighbourhood_cleansed (Top 10) ===\n")
dades_analisi %>%
  count(neighbourhood_cleansed, sort = TRUE) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  slice_head(n = 10) %>%
  print()

cat("\n=== property_type (Top 15) ===\n")
dades_analisi %>%
  count(property_type, sort = TRUE) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  slice_head(n = 15) %>%
  print()

cat("\n=== host_is_superhost ===\n")
dades_analisi %>%
  count(host_is_superhost, sort = TRUE) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  print()

cat("\n=== instant_bookable ===\n")
dades_analisi %>%
  count(instant_bookable, sort = TRUE) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  print()

cat("\n=== host_response_time ===\n")
dades_analisi %>%
  count(host_response_time, sort = TRUE) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  print()

cat("\n=== source ===\n")
dades_analisi %>%
  count(source, sort = TRUE) %>%
  mutate(Pct = round(n / sum(n) * 100, 2)) %>%
  print()


# ==============================================================================
# 3. AED Bivariant
# ==============================================================================

# ==============================================================================
# 3.1 Numèrica vs. Numèrica
# ==============================================================================

# Subconjunt de numèriques rellevants
vars_num_cor <- c("price", "accommodates", "bedrooms", "beds", "bathrooms",
                  "minimum_nights", "maximum_nights",
                  "availability_30", "availability_60",
                  "availability_90", "availability_365",
                  "number_of_reviews", "reviews_per_month",
                  "number_of_reviews_ltm", "number_of_reviews_l30d",
                  "estimated_occupancy_l365d", "estimated_revenue_l365d",
                  "host_response_rate", "host_acceptance_rate",
                  "host_listings_count",
                  "review_scores_rating", "review_scores_accuracy",
                  "review_scores_cleanliness", "review_scores_checkin",
                  "review_scores_communication", "review_scores_location",
                  "review_scores_value")

dades_num_cor <- dades_analisi %>% select(all_of(vars_num_cor))

# --- vis_cor (visdat): matriu de correlacions visual --------------------------
dades_num_cor %>%
  select(where(is.numeric)) %>%
  vis_cor(na_action = "complete.obs")

# --- plot_correlation (DataExplorer) ------------------------------------------
# Usa Pearson per defecte; na.omit per evitar errors amb NAs
plot_correlation(
  na.omit(dades_num_cor),
  type    = "continuous",
  maxcat  = 5L,
  title   = "Matriu de correlació — Variables numèriques"
)

# Versió amb Spearman
plot_correlation(
  na.omit(dades_num_cor),
  type    = "continuous",
  maxcat  = 5L,
  cor_args = list(method = "spearman"),
  title   = "Matriu de correlació — Variables numèriques"
)

# --- inspect_cor (inspectdf): correlacions ordenades per magnitud -------------
inspect_cor(dades_num_cor, with_col = "price") %>%
  show_plot()

inspect_cor(dades_num_cor, with_col = "review_scores_rating") %>%
  show_plot()

# Funció auxiliar per a correlació de Spearman amb una variable
plot_cor_spearman <- function(data, target_col) {
  data %>%
    select(where(is.numeric)) %>%
    cor(use = "pairwise.complete.obs", method = "spearman") %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    select(Variable, Correlacio = all_of(target_col)) %>%
    filter(Variable != target_col) %>%
    arrange(desc(abs(Correlacio))) %>%
    ggplot(aes(x = reorder(Variable, Correlacio),
               y = Correlacio,
               fill = Correlacio > 0)) +
    geom_col(alpha = 0.85) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#59A14F", "FALSE" = "#E15759"),
                      labels = c("Negativa", "Positiva")) +
    labs(title    = paste("Correlació amb", target_col),
         x = NULL, y = "Correlació", fill = "Direcció") +
    theme_minimal()
}

wrap_plots(
  plot_cor_spearman(dades_num_cor, "price"),
  plot_cor_spearman(dades_num_cor, "review_scores_rating"),
  ncol = 2
)

# --- Scatter plots seleccionats -----------------------------------------------

# Funció auxiliar per a scatter
# loess ajusta la corba al patró real de les dades
plot_scatter <- function(data, x_var, y_var) {
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(alpha = 0.3, size = 0.8, colour = "#4E79A7") +
    geom_smooth(method = "loess", se = TRUE,
                colour = "red", linewidth = 0.8, na.rm = TRUE) +
    labs(title = paste(y_var, "vs", x_var),
         x = x_var, y = y_var) +
    theme_minimal()
}

# Parelles d'interès
plot_scatter(dades_analisi, "accommodates",          "price")
plot_scatter(dades_analisi, "review_scores_rating",  "price")
plot_scatter(dades_analisi, "reviews_per_month",     "price")
plot_scatter(dades_analisi, "estimated_revenue_l365d", "price")
plot_scatter(dades_analisi, "estimated_occupancy_l365d", "estimated_revenue_l365d")
plot_scatter(dades_analisi, "number_of_reviews",     "review_scores_rating")
plot_scatter(dades_analisi, "host_acceptance_rate",  "review_scores_rating")
plot_scatter(dades_analisi, "bedrooms",              "price")

scatter_calls <- list(
  list("accommodates",             "price"),
  list("review_scores_rating",     "price"),
  list("reviews_per_month",        "price"),
  list("estimated_revenue_l365d",  "price"),
  list("estimated_occupancy_l365d","estimated_revenue_l365d"),
  list("number_of_reviews",        "review_scores_rating"),
  list("host_acceptance_rate",     "review_scores_rating"),
  list("bedrooms",                 "price")
)

plot_list <- list()
for (i in seq_along(scatter_calls)) {
  plot_list <- c(plot_list, list(
    plot_scatter(dades_analisi, scatter_calls[[i]][[1]], scatter_calls[[i]][[2]])
  ))
  if (i %% 4 == 0 || i == length(scatter_calls)) {
    print(wrap_plots(plot_list, ncol = 2))
    plot_list <- list()
  }
}

# ==============================================================================
# 3.2 Numèrica vs. Qualitativa
# ==============================================================================

# --- ExpNumViz (SmartEDA): boxplots agrupats per variable target ---------------

# Numèriques per room_type
ExpNumViz(
  dades_cat,
  target = "room_type",
  type   = 2,              # boxplot per grup
  nlim   = 25,
  fname  = NULL,
  Page   = c(3, 3)
)

# Numèriques per neighbourhood_cleansed (Top barris)
# Filtrem als 8 barris més freqüents per llegibilitat
barris_top8 <- dades_analisi %>%
  count(neighbourhood_cleansed, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(neighbourhood_cleansed)

dades_cat %>%
  filter(neighbourhood_cleansed %in% barris_top8) %>%
  ExpNumViz(
    target = "neighbourhood_cleansed",
    type   = 2,
    nlim   = 25,
    fname  = NULL,
    Page   = c(2, 2)
  )

# Numèriques per instant_bookable
ExpNumViz(
  dades_cat,
  target = "instant_bookable",
  type   = 2,
  nlim   = 25,
  fname  = NULL,
  Page   = c(2, 2)
)

# --- Boxplots ggplot2 per a les parelles més rellevants ----------------------

# Funció auxiliar boxplot agrupat
plot_boxplot_grup <- function(data, x_var, y_var, top_n = NULL) {
  df <- data
  if (!is.null(top_n)) {
    nivells <- df %>%
      count(.data[[x_var]], sort = TRUE) %>%
      slice_head(n = top_n) %>%
      pull(.data[[x_var]])
    df <- df %>% filter(.data[[x_var]] %in% nivells) %>%
      mutate(across(all_of(x_var), ~ droplevels(factor(.))))
  }
  ggplot(df, aes(x = reorder(.data[[x_var]], .data[[y_var]],
                             FUN = median, na.rm = TRUE),
                 y = .data[[y_var]])) +
    geom_boxplot(fill = "#4E79A7", alpha = 0.6,
                 outlier.colour = "red", outlier.size = 0.8,
                 na.rm = TRUE) +
    coord_flip() +
    labs(title = paste(y_var, "per", x_var),
         x = x_var, y = y_var) +
    theme_minimal()
}

# room_type: price | rating
print(wrap_plots(
  plot_boxplot_grup(dades_cat, "room_type", "price"),
  plot_boxplot_grup(dades_cat, "room_type", "review_scores_rating"),
  ncol = 2
))

# neighbourhood_cleansed (Top 10): price | rating
print(wrap_plots(
  plot_boxplot_grup(dades_cat, "neighbourhood_cleansed", "price", top_n = 10),
  plot_boxplot_grup(dades_cat, "neighbourhood_cleansed", "review_scores_rating", top_n = 10),
  ncol = 2
))

# property_type (Top 10): price | rating
print(wrap_plots(
  plot_boxplot_grup(dades_cat, "property_type", "price", top_n = 10),
  plot_boxplot_grup(dades_cat, "property_type", "review_scores_rating", top_n = 10),
  ncol = 2
))

# host_is_superhost: price | rating
print(wrap_plots(
  plot_boxplot_grup(dades_cat, "host_is_superhost", "price"),
  plot_boxplot_grup(dades_cat, "host_is_superhost", "review_scores_rating"),
  ncol = 2
))

# host_response_time: price | rating
print(wrap_plots(
  plot_boxplot_grup(dades_cat, "host_response_time", "price"),
  plot_boxplot_grup(dades_cat, "host_response_time", "review_scores_rating"),
  ncol = 2
))

# ==============================================================================
# 3.3 Qualitativa vs. Qualitativa
# ==============================================================================

# --- Funció auxiliar: taula de contingència + test Chi² ----------------------
taula_chi2 <- function(data, var1, var2) {
  cat("\n==============================\n")
  cat(paste("Contingència:", var1, "×", var2), "\n")
  cat("==============================\n")
  
  t <- table(data[[var1]], data[[var2]], useNA = "no")
  print(t)
  
  cat("\nPercentatges fila (%):\n")
  print(round(prop.table(t, margin = 1) * 100, 1))
  
  test <- chisq.test(t, simulate.p.value = TRUE)
  cat(paste0("\nChi² = ", round(test$statistic, 2),
             "  |  p-valor = ", format.pval(test$p.value, digits = 3),
             "  |  B = 2000 permutacions\n"))
}

# Parelles d'interès
taula_chi2(dades_cat, "room_type",            "host_is_superhost")
taula_chi2(dades_cat, "room_type",            "instant_bookable")
taula_chi2(dades_cat, "neighbourhood_cleansed","room_type")
taula_chi2(dades_cat, "host_is_superhost",    "host_response_time")
taula_chi2(dades_cat, "property_type",        "room_type")

# --- ExpCatViz (SmartEDA): stacked bar charts ---------------------------------

# Target: host_is_superhost
suppressWarnings(
  ExpCatViz(
    dades_cat %>% select(room_type, host_is_superhost,
                         instant_bookable, host_response_time,
                         property_type),
    target = "host_is_superhost",
    fname  = NULL, clim = 25, margin = 2, Page = c(2, 2)
  )
)

# Target: room_type (Top 8 barris)
suppressWarnings(
  dades_cat %>%
    filter(neighbourhood_cleansed %in% barris_top8) %>%
    select(neighbourhood_cleansed, room_type,
           host_is_superhost, instant_bookable) %>%
    ExpCatViz(
      target = "room_type",
      fname  = NULL, clim = 25, margin = 2, Page = c(2, 2)
    )
)

# --- Stacked bar ggplot2 per a les parelles més rellevants -------------------

# funció auxiliar
plot_stacked_bar <- function(data, x_var, fill_var, top_n = NULL) {
  df <- data %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[fill_var]]))
  if (!is.null(top_n)) {
    nivells <- df %>%
      count(.data[[x_var]], sort = TRUE) %>%
      slice_head(n = top_n) %>%
      pull(.data[[x_var]])
    df <- df %>% filter(.data[[x_var]] %in% nivells) %>%
      mutate(across(all_of(x_var), ~ droplevels(factor(.))))
  }
  ggplot(df, aes(x = reorder(.data[[x_var]], .data[[x_var]], FUN = length),
                 fill = .data[[fill_var]])) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    coord_flip() +
    labs(title = paste("Distribució de", fill_var, "per", x_var),
         x = NULL, y = "Proporció", fill = fill_var) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# neighbourhood_cleansed × room_type
print(plot_stacked_bar(dades_cat, "neighbourhood_cleansed", "room_type", top_n = 10))

# room_type × host_is_superhost | instant_bookable
print(
  plot_stacked_bar(dades_cat, "room_type", "host_is_superhost") +
    plot_stacked_bar(dades_cat, "room_type", "instant_bookable") +
    plot_layout(ncol = 2)
)

# host_is_superhost × host_response_time
print(plot_stacked_bar(dades_cat, "host_is_superhost", "host_response_time"))

# property_type × room_type (Top 10)
print(plot_stacked_bar(dades_cat, "property_type", "room_type", top_n = 10))

