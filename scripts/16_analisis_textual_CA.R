# ==============================================================================
# ANÀLISI TEXTUAL - CA SIMPLE
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LLIBRERIES
# ------------------------------------------------------------------------------
library(FactoMineR)
library(factoextra)
library(dplyr)
library(gridExtra)

# ------------------------------------------------------------------------------
# 2. CÀRREGA DE DADES PREPROCESSADES
# ------------------------------------------------------------------------------
dtm.matrix <- readRDS("../data/dtm_sample1000.rds")
reviews_ca <- readRDS("../data/reviews_ca_sample1000.rds")

cat("DTM carregada:", nrow(dtm.matrix), "reviews x", ncol(dtm.matrix), "termes\n")
cat("Metadades carregades:", nrow(reviews_ca), "reviews\n")

# ------------------------------------------------------------------------------
# 3. CA SIMPLE — Correspondence Analysis
# ------------------------------------------------------------------------------
# Files = reviews individuals, Columnes = termes

cat("\n===== CA SIMPLE =====\n")
res.ca <- CA(dtm.matrix, graph = FALSE)
res.ca

# -- 3.1 EIGENVALUES (mètode del colze) --
# -- 3.1 EIGENVALUES (mètode del colze) --
cat("\n--- EIGENVALUES ---\n")
print(head(res.ca$eig, 15)) # Utilitzem head() per evitar errors si hi ha menys de 15 dimensions

fviz_screeplot(res.ca,
               addlabels = TRUE,
               ylim      = c(0, 7),
               main      = "Scree Plot (Eigenvalues) — Mètode del Colze")

# -- 3.2 ANÀLISI PER FILES (Reviews) --
cat("\n--- ANÀLISI PER FILES (Reviews) ---\n")
row_ca <- get_ca_row(res.ca)
row_ca

p1 <- fviz_contrib(res.ca, choice = "row", axes = 1, top = 30,
                   title = "Contribució de Reviews — Dim 1\n(Línia vermella = contribució promig)")

p2 <- fviz_contrib(res.ca, choice = "row", axes = 2, top = 30,
                   title = "Contribució de Reviews — Dim 2")

grid.arrange(p1, p2, ncol = 2)

p1 <- fviz_contrib(res.ca, choice = "row", axes = 1:2, top = 30,
                   title = "Contribució de Reviews — Dim 1+2 (Interacció)")

p2 <- plot.CA(res.ca,
              invisible = "col",
              autoLab   = "yes",
              axes      = c(1, 2),
              title     = "Biplot Reviews — Dim 1 vs Dim 2\nReviews agrupades = comentaris amb vocabulari similar")

grid.arrange(p1, p2, ncol = 2)

# -- 3.3 ANÀLISI PER COLUMNES (Termes) --
cat("\n--- ANÀLISI PER COLUMNES (Termes) ---\n")
col_ca <- get_ca_col(res.ca)
col_ca

p1 <- fviz_contrib(res.ca, choice = "col", axes = 1, top = 30,
                   title = "Contribució de Termes — Dim 1\n(Paraules clau de la dimensió 1)")

p2 <- fviz_contrib(res.ca, choice = "col", axes = 2, top = 30,
                   title = "Contribució de Termes — Dim 2\n(Paraules clau de la dimensió 2)")

grid.arrange(p1, p2, ncol = 2)

p1 <- fviz_contrib(res.ca, choice = "col", axes = 1:2, top = 30,
                   title = "Contribució de Termes — Dim 1+2 (Interacció)")

p2 <- plot.CA(res.ca,
              invisible = "row",
              autoLab   = "yes",
              axes      = c(1, 2),
              title     = "Biplot Termes — Dim 1 vs Dim 2\nTermes agrupats = perfils d'opinió similars")

grid.arrange(p1, p2, ncol = 2)

# -- 3.4 BIPLOT SIMULTANI (Reviews + Termes) --
plot.CA(res.ca,
        autoLab   = "no",
        selectRow = "contrib 20",
        selectCol = "contrib 20",
        axes      = c(1, 2),
        title     = "Biplot Simultani — Dim 1 vs Dim 2\n(Top 20 reviews + top 20 termes per contribució)")