# ==============================================================================
# ALGORITME K-MEANS + JERÀRQUIC PER A BASES DE DADES GRANS (DADES MIXTES)
# K-proto (Gower^2) + Clustering Jeràrquic Ponderat dels Centroides
# ==============================================================================

library(dplyr)
library(cluster)      # daisy() i Gower
library(gower)        # gower_dist() punt a punt
library(clustMixType) # kproto() - extensió de K-means per a dades mixtes
library(factoextra)   # fviz_silhouette()

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓ INICIAL
# ------------------------------------------------------------------------------
K_gran  <- 17   # Nombre de clústers gran (rang recomanat: 14-20)
m_runs  <- 3    # Nombre d'execucions de K-proto (rang recomanat: 2-3)
k_final <- 3    # Nombre de clústers definitius (decidit après del dendrograma)

# Funció moda per a variables categòriques
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Heurística del cocient d'alçades per suggerir k òptim
suggested.level <- function(hc, min = 3, max = 10) {
  if (min < 2) stop("Min ha de ser >= 2")
  intra    <- rev(cumsum(hc$height))
  quot     <- intra[min:max] / intra[(min - 1):(max - 1)]
  nb_clust <- which.min(quot) + min - 1
  return(nb_clust)
}

# ------------------------------------------------------------------------------
# 1. CÀRREGA I PREPARACIÓ DE DADES
# ------------------------------------------------------------------------------
dataset <- readRDS("../data/dataset_feature_selection_final.rds")

data <- dataset %>%
  select(
    # Eliminem identificadors i variables textuals
    -id, -source, -host_id, -listing_url, -picture_url, -host_url,
    -host_thumbnail_url, -host_picture_url, -name, -description,
    -neighborhood_overview, -host_name, -host_location, -host_about,
    -host_verifications, -amenities, -license,
    # Eliminem coordenades (ja tenim el barri)
    -latitude, -longitude,
    # Eliminem el target
    -price, -log_price,
    # Eliminem originals quan existeix versió logarítmica
    -dies_antiguitat_listing, -dies_recencia_review,
    -estimated_revenue_l365d, -estimated_occupancy_l365d,
    -minimum_nights, -maximum_nights,
    -number_of_reviews, -reviews_per_month
  )

glimpse(data)

# ------------------------------------------------------------------------------
# 2. ETAPA 1 — m EXECUCIONS DE K-PROTO AMB K GRAN
# Usem llavors diferents per garantir diversitat entre particions.
# ------------------------------------------------------------------------------
llista_particions <- list()

for (run in 1:m_runs) {
  cat("Executant K-proto run", run, "de", m_runs, "...\n")
  set.seed(run * 100)
  
  kp <- kproto(
    x        = data,
    k        = K_gran,
    iter.max = 100,
    nstart   = 1,
    verbose  = FALSE
  )
  
  llista_particions[[run]] <- kp$cluster
  cat("  → Run", run, "completat. Mides dels clústers:\n")
  print(table(kp$cluster))
}

# ------------------------------------------------------------------------------
# 3. ETAPA 2 — TAULA CREUADA DE LES m PARTICIONS
# Cada combinació única d'etiquetes és una cel·la estable entre tots els runs.
# ------------------------------------------------------------------------------
etiquetes_combinades <- do.call(paste, c(llista_particions, sep = "_"))
taula_creuada        <- table(etiquetes_combinades)

cat("\n--- Resum de la Taula Creuada ---\n")
cat("Nombre total de cel·les no buides:", length(taula_creuada), "\n")
cat("Mida mínima de cel·la:           ", min(taula_creuada), "\n")
cat("Mida màxima de cel·la:           ", max(taula_creuada), "\n")
cat("Mida mitjana de cel·la:          ", round(mean(taula_creuada), 1), "\n")

cell_assignat <- as.integer(factor(etiquetes_combinades))
n_cells       <- max(cell_assignat)

# ------------------------------------------------------------------------------
# 4. ETAPA 3 — CENTROIDES DE LES CEL·LES NO BUIDES
# Numèriques: mitjana. Categòriques: moda.
# ------------------------------------------------------------------------------
centroides_cells <- data.frame()
mides_cells      <- numeric(n_cells)

for (cell_i in 1:n_cells) {
  idx                  <- which(cell_assignat == cell_i)
  mides_cells[cell_i] <- length(idx)
  subset_cell          <- data[idx, , drop = FALSE]
  centroide_i          <- list()
  
  for (col in colnames(subset_cell)) {
    if (is.numeric(subset_cell[[col]])) {
      centroide_i[[col]] <- mean(subset_cell[[col]], na.rm = TRUE)
    } else {
      centroide_i[[col]] <- get_mode(subset_cell[[col]])
    }
  }
  
  centroides_cells <- bind_rows(centroides_cells, as.data.frame(centroide_i))
}

cat("Centroides calculats:", nrow(centroides_cells), "\n")

# ------------------------------------------------------------------------------
# 5. ETAPA 4 — CLUSTERING JERÀRQUIC DELS CENTROIDES (Gower² + Ward.D2)
# Ponderem per mida de cel·la per reflectir el pes real de cada centroide.
# ------------------------------------------------------------------------------
matriu_gower_cents    <- daisy(centroides_cells, metric = "gower")
matriu_gower_sq_cents <- matriu_gower_cents^2

arbre_cents <- hclust(
  matriu_gower_sq_cents,
  method  = "ward.D2",
  members = mides_cells
)

plot(arbre_cents,
     labels = FALSE,
     main   = paste0("Dendrograma dels Centroides"),
     ylab   = "Alçada (Distància Gower²)")

k_suggerit <- suggested.level(arbre_cents, min = 3, max = 8)
cat("k suggerit per l'heurística del cocient d'alçades:", k_suggerit, "\n")

# Criteri del colze — guia visual per decidir k_final
plot(
  rev(arbre_cents$height)[1:20],
  type = "b", pch = 19, col = "steelblue",
  xlab = "Nombre de clústers",
  ylab = "Alçada de fusió",
  main = "Criteri del Colze — Alçades del Dendrograma"
)

k_final <- 3

abline(v = k_final, col = "tomato", lty = 2, lwd = 2)

# ------------------------------------------------------------------------------
# 6. ETAPA 5 — TALL DE L'ARBRE
# ------------------------------------------------------------------------------
cluster_cells <- cutree(arbre_cents, k = k_final)

cat("\nDistribució de cel·les per clúster final:\n")
print(table(cluster_cells))

plot(arbre_cents,
     labels = FALSE,
     main   = paste0("Dendrograma dels Centroides — k = ", k_final),
     ylab   = "Alçada (Distància Gower²)")
rect.hclust(arbre_cents, k = k_final, border = "tomato")

# ------------------------------------------------------------------------------
# 7. ETAPA 6 — CONSOLIDACIÓ
# Cada observació hereta el clúster de la seva cel·la. 
# ------------------------------------------------------------------------------
cluster_final <- cluster_cells[cell_assignat]

cat("\nDistribució final d'individus per clúster:\n")
print(table(cluster_final))

# ------------------------------------------------------------------------------
# 8. CENTROIDES FINALS I PERFILAT BÀSIC
# ------------------------------------------------------------------------------
data$cluster      <- cluster_final
centroides_finals <- data.frame()

for (i in 1:k_final) {
  subset_clust <- data[data$cluster == i, -ncol(data)]
  centroide_i  <- list()
  
  for (col in colnames(subset_clust)) {
    if (is.numeric(subset_clust[[col]])) {
      centroide_i[[col]] <- mean(subset_clust[[col]], na.rm = TRUE)
    } else {
      centroide_i[[col]] <- get_mode(subset_clust[[col]])
    }
  }
  
  centroide_df         <- as.data.frame(centroide_i)
  centroide_df$cluster <- i
  centroides_finals    <- bind_rows(centroides_finals, centroide_df)
}

cat("\nCentroides finals dels", k_final, "clústers:\n")
print(centroides_finals)

df_final <- dataset %>% mutate(cluster = cluster_final)

cat("\nDistribució final (dataset complet):\n")
print(table(df_final$cluster))

# ------------------------------------------------------------------------------
# 9. VALIDACIÓ — SILHOUETTE
# Silhouette global (totes les obs) + gràfic sobre mostra estratificada.
# ------------------------------------------------------------------------------
cat("\n--- Avaluació numèrica del model ---\n")

# 9a. Silhouette global (totes les 10.453 observacions)
dades_netes_global     <- data %>% select(-cluster)
dist_global            <- daisy(dades_netes_global, metric = "gower")
sil_global             <- silhouette(df_final$cluster, dist_global)
mitjana_silueta_global <- mean(sil_global[, 3])
cat("Silhouette score global:", round(mitjana_silueta_global, 4), "\n")

# 9b. Gràfic sobre mostra estratificada (1500 obs)
set.seed(123)
n_sil  <- 1500
prop   <- table(cluster_final) / nrow(data)
n_x_cl <- round(prop * n_sil)

idx_sil <- c()
for (i in 1:k_final) {
  idx_i   <- which(cluster_final == i)
  idx_sil <- c(idx_sil, sample(idx_i, min(n_x_cl[i], length(idx_i))))
}

data_sil     <- dades_netes_global[idx_sil, ]
cluster_sil  <- cluster_final[idx_sil]
dist_sil     <- daisy(data_sil, metric = "gower")
sil_obj_plot <- silhouette(cluster_sil, dist_sil)

grafic_sil <- fviz_silhouette(sil_obj_plot) +
  ggtitle(paste0("Silhouette — K_gran=", K_gran,
                 ", m=", m_runs, ", k=", k_final)) +
  theme_minimal()
print(grafic_sil)
print(summary(sil_obj_plot))

# 9c. Criteri de negoci — variança inter-clúster
mitjanes_cluster <- df_final %>%
  group_by(cluster) %>%
  summarise(
    ingressos_mitjans = mean(estimated_revenue_l365d, na.rm = TRUE),
    preu_mitja        = mean(price, na.rm = TRUE)
  )

cat("\nVariança d'ingressos entre clústers:", round(var(mitjanes_cluster$ingressos_mitjans), 2), "\n")
cat("Variança de preus entre clústers:   ", round(var(mitjanes_cluster$preu_mitja), 2), "\n")
cat("\nPerfilat de negoci:\n")
print(as.data.frame(mitjanes_cluster))

# ------------------------------------------------------------------------------
# 10. DESAR RESULTATS
# ------------------------------------------------------------------------------
saveRDS(df_final, file = "../data/dataset_kproto.rds")
cat("\nArxiu RDS guardat correctament.\n")