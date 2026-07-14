# ==============================================================================
# ANÀLISI TEXTUAL - CA-GALT
# ==============================================================================
# Correspondence Analysis on Generalised Aggregated Lexical Table
#
# Variables suplementàries:
#   · room_type                (tipus d'allotjament)
#   · zona_barri               (zonificació geogràfica d'Amsterdam, 7 zones)
#   · cat_review_scores_rating (nivell de puntuació de les ressenyes)
#
# Entrada : ../data/dtm_sample1000.rds + ../data/reviews_ca_sample1000.rds
#           + ../data/dataset_cure_python.rds
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LLIBRERIES
# ------------------------------------------------------------------------------
library(FactoMineR)   
library(factoextra)   
library(dplyr)
library(ggplot2)
library(gridExtra)

# ------------------------------------------------------------------------------
# 2. CÀRREGA DE DADES
# ------------------------------------------------------------------------------
dtm.matrix   <- readRDS("../data/dtm_sample1000.rds")
reviews_ca   <- readRDS("../data/reviews_ca_sample1000.rds")
dataset_main <- readRDS("../data/dataset_cure_python.rds")

cat("DTM carregada          :", nrow(dtm.matrix), "reviews x", ncol(dtm.matrix), "termes\n")
cat("Metadades carregades   :", nrow(reviews_ca),  "reviews\n")
cat("Dataset principal      :", nrow(dataset_main), "listings\n")

# ------------------------------------------------------------------------------
# 3. FUNCIÓ DE ZONIFICACIÓ GEOGRÀFICA
# ------------------------------------------------------------------------------
# Agrupa els 22 barris de neighbourhood_cleansed en 7 zones geogràfiques.

zona_barri <- function(neighbourhood) {
  centre     <- c("Centrum-West", "Centrum-Oost")
  zuid       <- c("De Pijp - Rivierenbuurt", "Zuid", "Buitenveldert - Zuidas")
  oost       <- c("Oud-Oost", "Watergraafsmeer",
                  "Oostelijk Havengebied - Indische Buurt",
                  "IJburg - Zeeburgereiland")
  west       <- c("De Baarsjes - Oud-West", "Bos en Lommer", "Westerpark")
  nieuw_west <- c("Geuzenveld - Slotermeer", "Slotervaart",
                  "De Aker - Nieuw Sloten", "Osdorp")
  noord      <- c("Noord-West", "Noord-Oost", "Oud-Noord")
  case_when(
    neighbourhood %in% centre     ~ "Centre",
    neighbourhood %in% zuid       ~ "Zuid",
    neighbourhood %in% oost       ~ "Oost",
    neighbourhood %in% west       ~ "West",
    neighbourhood %in% nieuw_west ~ "Nieuw_West",
    neighbourhood %in% noord      ~ "Noord",
    TRUE                          ~ "Zuidoost"
  )
}

# ------------------------------------------------------------------------------
# 4. PREPARACIÓ DE VARIABLES SUPLEMENTÀRIES
# ------------------------------------------------------------------------------
vars_cagalt <- dataset_main %>%
  select(id,
         room_type,
         neighbourhood_cleansed,
         cat_review_scores_rating) %>%
  mutate(
    room_binary              = factor(ifelse(room_type == "Entire home/apt",
                                             "Entire", "Room"),
                                      levels = c("Room", "Entire")),
    zona_barri               = factor(zona_barri(neighbourhood_cleansed)),
    cat_review_scores_rating = factor(cat_review_scores_rating)
  ) %>%
  select(-neighbourhood_cleansed,  -room_type) # eliminem versió original

cat("\nVariables suplementàries preparades:", ncol(vars_cagalt) - 1, "variables\n")
cat("Modalitats per variable:\n")
for (v in names(vars_cagalt)[-1]) {
  cat("  ", formatC(v, width = 28, flag = "-"),
      ":", nlevels(vars_cagalt[[v]]), "modalitats —",
      paste(levels(vars_cagalt[[v]]), collapse = ", "), "\n")
}

# ------------------------------------------------------------------------------
# 5. JOIN: reviews_ca <-> dataset_main
# ------------------------------------------------------------------------------
# Cada review porta listing_id, fem el join amb el dataset principal.

reviews_enriched <- reviews_ca %>%
  mutate(listing_id = as.character(listing_id)) %>%
  left_join(
    vars_cagalt %>% mutate(id = as.character(id)),
    by = c("listing_id" = "id")
  )

cat("\nDimensió reviews_enriched:",
    nrow(reviews_enriched), "reviews x", ncol(reviews_enriched), "columnes\n")

# -- Diagnòstic del join --
vars_model <- c("room_binary", "zona_barri", "cat_review_scores_rating")

na_check <- reviews_enriched %>%
  summarise(across(all_of(vars_model), ~sum(is.na(.))))
cat("\nNAs per variable suplementària (esperats = 0):\n")
print(na_check)

# -- Alineació dtm.matrix <-> reviews_enriched --
# Les dues estructures han de tenir el mateix nombre de files i el mateix ordre.
stopifnot(nrow(reviews_enriched) == nrow(dtm.matrix))
cat("✓ Alineació dtm.matrix <-> reviews_enriched correcta\n")

# ------------------------------------------------------------------------------
# 6. FUNCIONS AUXILIARS
# ------------------------------------------------------------------------------

# -- 6.1 Preparació i neteja del bloc d'entrada per a CaGalt() --
# CaGalt() no accepta NAs a X, eliminem files incompletes i sincronitzem DTM.

prep_cagalt <- function(reviews_df, dtm_mat, vars) {
  keep <- complete.cases(reviews_df[, vars])
  cat("Files vàlides (sense NA):", sum(keep),
      "| Eliminades per NA:", sum(!keep), "\n")
  if (sum(!keep) > 0) {
    cat("  Listings sense match al dataset principal:", sum(!keep), "\n")
  }
  list(
    dtm   = dtm_mat[keep, ],
    X     = droplevels(as.data.frame(reviews_df[keep, vars])),
    n     = sum(keep),
    drops = sum(!keep)
  )
}

# -- 6.2 Scree Plot personalitzat --
scree_cagalt <- function(res, titol = "Scree Plot CA-GALT") {
  eig      <- as.data.frame(res$eig)
  n_dims   <- nrow(eig)
  eig$comp <- factor(paste0("Dim", seq_len(n_dims)),
                     levels = paste0("Dim", seq_len(n_dims)))
  acum2    <- round(eig[min(2, n_dims), "cumulative percentage of variance"], 1)
  
  ggplot(eig, aes(x = comp, y = `percentage of variance`)) +
    geom_col(fill = "#4393C3", width = 0.55, alpha = 0.85) +
    geom_text(aes(label = paste0(round(`percentage of variance`, 1), "%")),
              vjust = -0.5, size = 3.2) +
    geom_line(aes(group = 1), colour = "#D6604D", linewidth = 0.8) +
    geom_point(colour = "#D6604D", size = 2.5) +
    geom_vline(xintercept = 2.5, linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    annotate("text", x = 2.7,
             y = max(eig$`percentage of variance`) * 1.1,
             label = paste0("Dim1+2 = ", acum2, "%"),
             hjust = 0, size = 3.2, colour = "grey40") +
    ylim(0, max(eig$`percentage of variance`) * 1.25) +
    labs(title    = titol,
         subtitle = paste0("Variància acumulada Dim1+Dim2: ", acum2, "%"),
         x = "Dimensió", y = "% Variància explicada") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# -- 6.3 Taula consolidada de coordenades + cos² de modalitats --
tabla_modalitats <- function(res, dp = 1:2) {
  coord <- as.data.frame(res$quali.var$coord[, dp, drop = FALSE])
  cos2  <- as.data.frame(res$quali.var$cos2[,  dp, drop = FALSE])
  colnames(cos2) <- paste0("cos2_", colnames(cos2))
  cbind(round(coord, 4), round(cos2, 4))
}

# ------------------------------------------------------------------------------
# 7. MODEL CA-GALT
# ------------------------------------------------------------------------------
bloc <- prep_cagalt(reviews_enriched, dtm.matrix, vars_model)

cat("\n===== EXECUTANT CA-GALT =====\n")
cat("  DTM              :", nrow(bloc$dtm), "reviews x", ncol(bloc$dtm), "termes\n")
cat("  Variables suplem.:", ncol(bloc$X), "variables\n")
cat("  Modalitats totals:", sum(sapply(bloc$X, nlevels)), "\n")

res.cagalt <- CaGalt(Y     = bloc$dtm,
                     X     = bloc$X,
                     type  = "n",   # "n" = variables categòriques (nominales)
                     graph = FALSE)

cat("✓ CA-GALT completat\n")

# ------------------------------------------------------------------------------
# 8. DIAGNÒSTIC I RESULTATS NUMÈRICS
# ------------------------------------------------------------------------------

# -- 8.1 Eigenvalues + Scree Plot --
cat("\n--- EIGENVALUES CA-GALT ---\n")
print(round(res.cagalt$eig, 4))
scree_cagalt(res.cagalt, "Scree Plot CA-GALT — Mètode del Colze")

dp <- 1:2   # pla principal retingut (Dim1 + Dim2)

# -- 8.2 Termes: top 20 per contribució combinada Dim1+Dim2 --
cat("\n--- TOP 20 TERMES (contribució conjunta Dim1+Dim2) ---\n")
contr_combined <- apply(res.cagalt$freq$contr[, dp, drop = FALSE], 1, sum)
print(round(
  res.cagalt$freq$contr[
    order(contr_combined, decreasing = TRUE)[1:20], dp, drop = FALSE
  ], 4))

# -- 8.3 Termes: extrems per eix  --
coords_freq <- res.cagalt$freq$coord[, dp, drop = FALSE]

cat("\n--- TOP 10 TERMES Dim1 ---\n")
cat("  (+) Funcional / Estructural:\n")
print(round(head(coords_freq[order(coords_freq[, 1], decreasing = TRUE),  ], 10), 4))
cat("  (-) Emocional / Subjectiu:\n")
print(round(head(coords_freq[order(coords_freq[, 1], decreasing = FALSE), ], 10), 4))

cat("\n--- TOP 10 TERMES Dim2 ---\n")
cat("  (+) Propietat / Domèstic:\n")
print(round(head(coords_freq[order(coords_freq[, 2], decreasing = TRUE),  ], 10), 4))
cat("  (-) Localització / Mobilitat:\n")
print(round(head(coords_freq[order(coords_freq[, 2], decreasing = FALSE), ], 10), 4))

# -- 8.4 Variables suplementàries: coordenades + cos² --
cat("\n--- VARIABLES SUPLEMENTÀRIES: coordenades + cos² ---\n")
print(tabla_modalitats(res.cagalt, dp))

# -- 8.5 Summary complet --
cat("\n===== SUMMARY CA-GALT =====\n")
summary(res.cagalt)

# ------------------------------------------------------------------------------
# 9. VISUALITZACIONS
# ------------------------------------------------------------------------------
# Escales comunes per als panells comparatius

lims_freq <- range(res.cagalt$freq$coord[, dp],        na.rm = TRUE) * 1.25
lims_qv   <- range(res.cagalt$quali.var$coord[, dp],  na.rm = TRUE) * 1.25
lims_tots <- range(c(lims_freq, lims_qv)) * 1.1

# -- 9.1 Termes: top 20 per contribució --
plot.CaGalt(res.cagalt,
            choix  = "freq",
            axes   = c(1, 2),
            select = "contrib 20",
            title  = "CA-GALT — Termes (top 20 per contribució)")

# -- 9.2 Termes: top 20 per cos² (qualitat de representació) --
plot.CaGalt(res.cagalt,
            choix  = "freq",
            axes   = c(1, 2),
            select = "cos2 20",
            title  = "CA-GALT — Termes (top 20 per cos²)")

# -- 9.3 Modalitats suplementàries --
plot.CaGalt(res.cagalt,
            choix   = "quali.var",
            axes    = c(1, 2),
            autoLab = "yes",
            title   = "CA-GALT — Modalitats suplementàries")

# -- 9.4 Modalitats amb el·lipses de confiança --
plot(res.cagalt,
     choix      = "quali.var",
     conf.ellip = TRUE,
     axes       = c(1, 2),
     title      = "CA-GALT — Modalitats amb el·lipses de confiança")

# -- 9.5 Panell triple amb escala comuna --

par(mfrow = c(1, 3), pty = "s", mar = c(4, 4, 3, 1))

plot.CaGalt(res.cagalt,
            choix  = "ind",
            select = "cos2 10",
            axes   = c(1, 2),
            xlim   = lims_tots, ylim = lims_tots,
            title  = "Reviews (cos² 10)")

plot.CaGalt(res.cagalt,
            choix  = "freq",
            select = "contrib 15",
            axes   = c(1, 2),
            xlim   = lims_tots, ylim = lims_tots,
            title  = "Termes (contrib 15)")

plot.CaGalt(res.cagalt,
            choix      = "quali.var",
            conf.ellip = FALSE,
            select     = "cos2 6",
            axes       = c(1, 2),
            xlim       = lims_tots, ylim = lims_tots,
            title      = "Variables suplem. (cos² 6)")

par(mfrow = c(1, 1), pty = "m", mar = c(5, 4, 4, 2))

# -- 9.6 Panell doble amb escala comuna --

par(mfrow = c(1, 2), pty = "s", mar = c(4, 4, 2, 1))

plot.CaGalt(res.cagalt,
            choix  = "freq",
            select = "contrib 15",
            axes   = c(1, 2),
            xlim   = lims_tots, ylim = lims_tots,
            title  = "Termes (top 15 per contribució)")

plot.CaGalt(res.cagalt,
            choix      = "quali.var",
            conf.ellip = FALSE,
            autoLab    = "yes",          # evita solapament
            axes       = c(1, 2),
            xlim       = lims_tots, ylim = lims_tots,
            title      = "Variables suplementàries")

par(mfrow = c(1, 1), pty = "m", mar = c(5, 4, 4, 2))

# ------------------------------------------------------------------------------
# 10. ANÀLISI PER DOCUMENTS (ressenyes individuals)
# ------------------------------------------------------------------------------

dp <- 1:2

# -- 10.1 Qualitat de representació dels individus --
ind_coord <- as.data.frame(res.cagalt$ind$coord[, dp])
ind_cos2  <- as.data.frame(res.cagalt$ind$cos2[,  dp])
colnames(ind_coord) <- c("Dim1", "Dim2")
colnames(ind_cos2)  <- c("cos2_Dim1", "cos2_Dim2")

ind_df <- cbind(
  reviews_enriched[, c("listing_id", "room_binary",
                       "zona_barri", "cat_review_scores_rating")],
  ind_coord,
  ind_cos2
) %>%
  mutate(cos2_total = cos2_Dim1 + cos2_Dim2)

cat("\n--- QUALITAT DE REPRESENTACIÓ GLOBAL ---\n")
cat(sprintf("  Ressenyes amb cos²_total > 0.50 : %d (%.1f%%)\n",
            sum(ind_df$cos2_total > 0.50),
            100 * mean(ind_df$cos2_total > 0.50)))
cat(sprintf("  Ressenyes amb cos²_total > 0.25 : %d (%.1f%%)\n",
            sum(ind_df$cos2_total > 0.25),
            100 * mean(ind_df$cos2_total > 0.25)))
cat(sprintf("  cos² mitjà (Dim1+Dim2)          : %.3f\n",
            mean(ind_df$cos2_total)))

# -- 10.2 Top 15 ressenyes millor representades --
cat("\n--- TOP 15 RESSENYES (cos² Dim1+Dim2 més alt) ---\n")
top15 <- ind_df %>%
  arrange(desc(cos2_total)) %>%
  slice(1:15) %>%
  select(listing_id, room_binary, zona_barri,
         cat_review_scores_rating, Dim1, Dim2, cos2_total)
print(round(top15 %>% select(-listing_id, -room_binary,
                             -zona_barri, -cat_review_scores_rating), 3),
      row.names = FALSE)
print(top15 %>% select(listing_id, room_binary, zona_barri,
                       cat_review_scores_rating))

# -- 10.3 Ressenyes extremes per eix (ancoratge empíric) --
cat("\n--- RESSENYES EXTREMES Dim1 ---\n")
cat("  Pol (+) Crític / Room:\n")
ext_dim1_pos <- ind_df %>% arrange(desc(Dim1)) %>% slice(1:5) %>%
  select(listing_id, room_binary, zona_barri, cat_review_scores_rating, Dim1, cos2_total)
print(ext_dim1_pos)

cat("  Pol (-) Experiencial / Entire:\n")
ext_dim1_neg <- ind_df %>% arrange(Dim1) %>% slice(1:5) %>%
  select(listing_id, room_binary, zona_barri, cat_review_scores_rating, Dim1, cos2_total)
print(ext_dim1_neg)

cat("\n--- RESSENYES EXTREMES Dim2 ---\n")
cat("  Pol (+) Perifèria / Transport:\n")
ext_dim2_pos <- ind_df %>% arrange(desc(Dim2)) %>% slice(1:5) %>%
  select(listing_id, room_binary, zona_barri, cat_review_scores_rating, Dim2, cos2_total)
print(ext_dim2_pos)

cat("  Pol (-) Centre turístic:\n")
ext_dim2_neg <- ind_df %>% arrange(Dim2) %>% slice(1:5) %>%
  select(listing_id, room_binary, zona_barri, cat_review_scores_rating, Dim2, cos2_total)
print(ext_dim2_neg)

# -- 10.4 Distribució de coordenades per modalitat (box plots) --

p_dim1 <- ggplot(ind_df,
                 aes(x = room_binary, y = Dim1, fill = room_binary)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c("Room" = "#D6604D", "Entire" = "#4393C3")) +
  labs(title = "Dim1 per room_binary",
       x = NULL, y = "Coordenada Dim1") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

p_dim1_rat <- ggplot(ind_df,
                     aes(x = cat_review_scores_rating, y = Dim1,
                         fill = cat_review_scores_rating)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title = "Dim1 per cat_rating",
       x = NULL, y = "Coordenada Dim1") +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

p_dim2 <- ggplot(ind_df,
                 aes(x = zona_barri, y = Dim2, fill = zona_barri)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title = "Dim2 per zona_barri",
       x = NULL, y = "Coordenada Dim2") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

grid.arrange(p_dim1, p_dim1_rat, p_dim2, ncol = 3,
             top = "Distribució de coordenades individuals per modalitat")

# -- 10.5 Verificació del text de les ressenyes extremes --
# Mostra el text de les 3 ressenyes més extremes de cada pol per validar
# que el vocabulari identificat és coherent amb la interpretació dels eixos.

cat("\n--- TEXT DE RESSENYES EXTREMES (validació qualitativa) ---\n")

idx_top_dim1 <- order(ind_df$Dim1, decreasing = TRUE)[1:3]
idx_bot_dim1 <- order(ind_df$Dim1, decreasing = FALSE)[1:3]
idx_top_dim2 <- order(ind_df$Dim2, decreasing = TRUE)[1:3]
idx_bot_dim2 <- order(ind_df$Dim2, decreasing = FALSE)[1:3]

mostrar_textos <- function(idx, etiqueta) {
  cat(sprintf("\n  [%s]\n", etiqueta))
  for (i in idx) {
    cat(sprintf("  · listing %s | %s | %s | %s\n    %s\n",
                reviews_enriched$listing_id[i],
                ind_df$room_binary[i],
                ind_df$zona_barri[i],
                ind_df$cat_review_scores_rating[i],
                substr(reviews_enriched$comments[i], 1, 200)))
  }
}

mostrar_textos(idx_top_dim1, "Dim1 (+) Crític / Room")
mostrar_textos(idx_bot_dim1, "Dim1 (-) Experiencial / Entire")
mostrar_textos(idx_top_dim2, "Dim2 (+) Perifèria / Transport")
mostrar_textos(idx_bot_dim2, "Dim2 (-) Centre turístic")