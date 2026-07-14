# ==============================================================================
# ANÀLISI TEXTUAL - LDA (LATENT DIRICHLET ALLOCATION)
# ==============================================================================
# Entrada : ../data/dtm_sample1000.rds         (matriu DTM TF, 1000 reviews x termes)
#           ../data/reviews_ca_sample1000.rds  (metadades alineades)
# Sortida : ../data/lda_model.rds              (model LDA final)
#           ../data/lda_theta.rds              (Theta d: distribució tòpics per document)
#           ../data/lda_phi.rds                (Phi k:  distribució paraules per tòpic)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LLIBRERIES
# ------------------------------------------------------------------------------
library(remotes)
if (!requireNamespace("ldatuning", quietly = TRUE)) {
  remotes::install_github("nikita-moor/ldatuning")
}
library(ldatuning)

library(lda)
library(tm)
library(topicmodels)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(reshape2)
library(gridExtra)

# ------------------------------------------------------------------------------
# 2. CÀRREGA DE DADES PREPROCESSADES
# ------------------------------------------------------------------------------
# Matriu DTM (TF) generada al Preprocessament
dtm.matrix  <- readRDS("../data/dtm_sample1000.rds")
reviews_lda <- readRDS("../data/reviews_ca_sample1000.rds")

cat("DTM carregada:", nrow(dtm.matrix), "reviews x", ncol(dtm.matrix), "termes\n")
cat("Metadades carregades:", nrow(reviews_lda), "reviews\n")

# ------------------------------------------------------------------------------
# 3. CONVERSIÓ DTM PER LDA
# ------------------------------------------------------------------------------
# LDA (topicmodels) requereix un objecte DocumentTermMatrix de tm.
# Eliminem files amb suma zero (LDA no admet files buides)

DTM <- as.DocumentTermMatrix(
  Matrix::sparseMatrix(
    i        = row(dtm.matrix)[dtm.matrix > 0],
    j        = col(dtm.matrix)[dtm.matrix > 0],
    x        = dtm.matrix[dtm.matrix > 0],
    dims     = dim(dtm.matrix),
    dimnames = dimnames(dtm.matrix)
  ),
  weighting = weightTf
)

sel_idx     <- slam::row_sums(DTM) > 0
DTM         <- DTM[sel_idx, ]
reviews_lda <- reviews_lda[sel_idx, ]

cat("DTM per LDA:", nrow(DTM), "documents x", ncol(DTM), "termes\n")
cat("Reviews conservades:", nrow(reviews_lda), "\n")

# ------------------------------------------------------------------------------
# 4. K INICIAL — NÚVOL DE PARAULES EXPLORATORI
# ------------------------------------------------------------------------------
# Inspeccionem visualment el vocabulari per fer una estimació
# inicial del nombre de tòpics abans d'executar ldatuning

cat("\n===== K INICIAL: NÚVOL DE PARAULES EXPLORATORI =====\n")

freq        <- colSums(dtm.matrix)
freq_sorted <- sort(freq, decreasing = TRUE)

mycolors <- brewer.pal(8, "Dark2")
wordcloud(
  words        = names(freq_sorted),
  freq         = freq_sorted,
  max.words    = 100,
  random.order = FALSE,
  colors       = mycolors
)
title(main = "Núvol de paraules global — Estimació visual de k inicial")

# Grups identificats visualment: experiència positiva, localització/transport,
# descripció física, host/comunicació -> k inicial estimat: 4-6

# ------------------------------------------------------------------------------
# 5. SELECCIÓ DE K — CRITERI DE GIBBS (ldatuning)
# ------------------------------------------------------------------------------
# CaoJuan2009 -> minimitzar (similitud mínima entre tòpics)
# Deveaud2014 -> maximitzar (màxima diferència entre distribucions de tòpics)
# K òptim: on convergeixen ambdues mètriques

cat("\n===== SELECCIÓ DE K (ldatuning — Gibbs) =====\n")
cat("Explorant K de 2 a 16...\n")

result_k <- ldatuning::FindTopicsNumber(
  DTM,
  topics  = seq(from = 2, to = 16, by = 2),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method  = "Gibbs",
  control = list(seed = 42),
  verbose = TRUE
)

FindTopicsNumber_plot(result_k)

# Resultat obtingut: K = 6
K <- 6

cat("\nK seleccionat:", K, "tòpics\n")

# ------------------------------------------------------------------------------
# 6. AJUST DEL MODEL LDA
# ------------------------------------------------------------------------------
# Mètode: Gibbs Sampling | alpha = 0.2 | iter = 500

cat("\n===== AJUST DEL MODEL LDA =====\n")

set.seed(42)
lda_model <- LDA(
  DTM,
  k       = K,
  method  = "Gibbs",
  control = list(
    iter    = 500,
    verbose = 25,
    alpha   = 0.2
  )
)

lda_model
cat("\nAlpha del model:", attr(lda_model, "alpha"), "\n")

# ------------------------------------------------------------------------------
# 7. EXTRACCIÓ DE RESULTATS POSTERIORS
# ------------------------------------------------------------------------------
# Phi k    distribució de paraules per tòpic -> K × termes
#          Defineix la semàntica / latència de cada tòpic
#
# Theta d: distribució de tòpics per document -> documents × K
#          Indica amb quina probabilitat cada document pertany a cada tòpic

cat("\n===== DISTRIBUCIONS POSTERIORS =====\n")

tmResult <- posterior(lda_model)
attributes(tmResult)

phi   <- tmResult$terms
theta <- tmResult$topics

cat("Dimensió Phi   (tòpics × termes)    :", nrow(phi),   "x", ncol(phi),   "\n")
cat("Dimensió Theta (documents × tòpics) :", nrow(theta), "x", ncol(theta), "\n")

cat("\n--- Top 10 termes per tòpic (Phi k) ---\n")
print(terms(lda_model, 10))

top5       <- terms(lda_model, 5)
topicNames <- apply(top5, 2, paste, collapse = " ")

cat("\nEtiquetes de tòpics:\n")
print(topicNames)

cat("\nResum distribució Theta (tòpics per document):\n")
summary(theta)

# ------------------------------------------------------------------------------
# 8. ANÀLISI PHI k — PARAULES PER TÒPIC
# ------------------------------------------------------------------------------

cat("\n===== ANÀLISI PHI k (paraules per tòpic) =====\n")

# tidy() anomena la matriu "beta" per conveni de tidytext
lda_phi_tidy <- tidy(lda_model, matrix = "beta")
cat("Dimensió Phi tidy:", nrow(lda_phi_tidy), "files\n")

# -- 8.1 Top 10 paraules per tòpic (barchart) --
top_terms <- lda_phi_tidy %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  scale_y_reordered() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Top 10 paraules per tòpic — Probabilitat Phi k",
    subtitle = paste0("LDA amb K = ", K, " tòpics | Gibbs Sampling, iter = 500, alpha = 0.2"),
    x        = "Probabilitat Phi k",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

# -- 8.2 Log-ràtio Phi k entre tòpic 1 i tòpic 2 (paraules diferenciadores) --
beta_wide <- lda_phi_tidy %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

cat("\n--- Top paraules diferenciadores (log-ràtio tòpic2/tòpic1) ---\n")
print(beta_wide %>% arrange(desc(abs(log_ratio))) %>% head(20))

par(mfrow = c(2, 3))

# -- 8.3 Núvol de paraules per cada tòpic --
for (topicToViz in 1:K) {
  top50terms <- sort(phi[topicToViz, ], decreasing = TRUE)[1:50]
  words_viz  <- names(top50terms)
  probs_viz  <- as.numeric(top50terms)
  
  wordcloud(
    words_viz, probs_viz,
    random.order = FALSE,
    colors       = mycolors
  )
  title(main = paste0("Tòpic ", topicToViz, " — ", topicNames[topicToViz]))
}

par(mfrow = c(1, 1))

# -- 8.4 Heatmap similitud cosinus entre tòpics — Phi k --
# Comprovació de solapament entre tòpics
# Volem valors baixos fora de la diagonal (tòpics discriminatius)

cosine_sim <- function(mat) {
  norm     <- sqrt(rowSums(mat^2))
  mat_norm <- mat / norm
  mat_norm %*% t(mat_norm)
}

sim_matrix <- cosine_sim(phi)
rownames(sim_matrix) <- paste0("T", 1:K)
colnames(sim_matrix) <- paste0("T", 1:K)

cat("\n--- Matriu de similitud cosinus entre tòpics (Phi k) ---\n")
print(round(sim_matrix, 3))

sim_melt <- reshape2::melt(
  sim_matrix,
  varnames   = c("Topic_i", "Topic_j"),
  value.name = "Similitud"
)

ggplot(sim_melt, aes(x = Topic_i, y = Topic_j, fill = Similitud)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Similitud, 2)), size = 3.5) +
  scale_fill_gradient(low = "#f7fbff", high = "#08519c") +
  labs(
    title    = "Heatmap similitud cosinus entre tòpics — Phi k",
    subtitle = "Diagonal = 1 (idèntic). Fora diagonal: volem valors baixos (tòpics discriminatius)",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11)

# ------------------------------------------------------------------------------
# 9. ANÀLISI THETA d — TÒPICS PER DOCUMENT
# ------------------------------------------------------------------------------

cat("\n===== ANÀLISI THETA d (tòpics per document) =====\n")

lda_theta_tidy <- tidy(lda_model, matrix = "gamma")
cat("Dimensió Theta tidy:", nrow(lda_theta_tidy), "files\n")

cat("\n--- Primeres files Theta ---\n")
print(head(lda_theta_tidy, 12))

dominant_topic <- lda_theta_tidy %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup()

cat("\n--- Distribució de tòpics dominants entre documents ---\n")
print(table(dominant_topic$topic))

# -- 9.1 Composició de tòpics en documents exemple --
# Distribucions Theta similars entre documents indiquen documents semblants

exampleIds <- c(2, 100, 200)
N          <- length(exampleIds)

topicProportionExamples           <- theta[exampleIds, ]
colnames(topicProportionExamples) <- topicNames

vizDataFrame <- reshape2::melt(
  cbind(
    data.frame(topicProportionExamples),
    document = factor(1:N)
  ),
  variable.name = "topic",
  id.vars       = "document"
)

ggplot(data = vizDataFrame,
       aes(topic, value, fill = document)) +
  geom_bar(stat = "identity") +
  labs(
    title    = "Distribució Theta d — 3 documents exemple",
    subtitle = "Distribucions similars entre documents indiquen documents semblants",
    x        = NULL,
    y        = "Proporció Theta d"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~ document, ncol = N)

# -- 9.2 Heatmap Theta d: documents × tòpics (submostra de 50 documents) --
theta_sub <- theta[1:min(50, nrow(theta)), ]
colnames(theta_sub) <- paste0("T", 1:K, ": ", substr(topicNames, 1, 15))

theta_melt <- reshape2::melt(
  theta_sub,
  varnames   = c("Document", "Topic"),
  value.name = "Theta"
)

ggplot(theta_melt, aes(x = Topic, y = factor(Document), fill = Theta)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#f7fbff", high = "#08519c", name = "Theta d") +
  labs(
    title    = "Heatmap Theta d — Distribució de tòpics (primers 50 documents)",
    subtitle = "Cada cel·la = probabilitat que el document pertanyi al tòpic",
    x        = "Tòpic",
    y        = "Document"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x  = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid   = element_blank()
  )

# ------------------------------------------------------------------------------
# 10. FILTRAT DE DOCUMENTS PER TÒPIC (llindar Theta >= 0.6)
# ------------------------------------------------------------------------------
# Documents amb Theta >= 0.6 per un tòpic comparteixen distribució
# similar -> documents semblants associats a aquell tòpic

cat("\n===== FILTRAT DE DOCUMENTS PER TÒPIC (Theta >= 0.6) =====\n")

THRESHOLD <- 0.6

for (t in 1:K) {
  sel_t <- which(theta[, t] >= THRESHOLD)
  cat("Tòpic", t, paste0("(", topicNames[t], ")"),
      "->", length(sel_t), "documents amb Theta >=", THRESHOLD, "\n")
}

# Exemple detallat: documents del tòpic 1
topicToFilter <- 1
selectedIdx   <- which(theta[, topicToFilter] >= THRESHOLD)

cat("\nDocuments seleccionats (tòpic", topicToFilter,
    ", Theta >=", THRESHOLD, "):", length(selectedIdx), "\n")

cat("\n--- Paraules més freqüents del document 6 ---\n")
tidy(DTM) %>%
  filter(document == 6) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# 11. DESAR
# ------------------------------------------------------------------------------
saveRDS(lda_model, "../data/lda_model.rds")
saveRDS(theta,     "../data/lda_theta.rds")
saveRDS(phi,       "../data/lda_phi.rds")

cat("\nDesat:\n")
cat(" ../data/lda_model.rds — model LDA (K =", K, ")\n")
cat(" ../data/lda_theta.rds — Theta d:", nrow(theta), "x", ncol(theta), "\n")
cat(" ../data/lda_phi.rds   — Phi k:  ", nrow(phi),   "x", ncol(phi),   "\n")