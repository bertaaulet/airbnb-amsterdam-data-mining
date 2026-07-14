library(tidyverse)
library(lubridate)
library(dtwclust)
library(dendextend)
library(proxy)
library(cluster)

# PAS 1

df_serie <- read_csv("../data/dataset_time_series.csv")

df_serie_clean <- df_serie %>%
  rename(Country = 1) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -Country, names_to = "Fecha", values_to = "Pernoctaciones") %>%
  mutate(
    Pernoctaciones = as.numeric(str_replace_all(Pernoctaciones, "\\.", "")),
    Fecha = dmy(paste0("01-", Fecha))
  )

df_serie_wide <- df_serie_clean %>%
  arrange(Fecha) %>%
  pivot_wider(names_from = Fecha, values_from = Pernoctaciones)

dades_ts_num <- as.matrix(df_serie_wide[, -1])
rownames(dades_ts_num) <- df_serie_wide$Country

cat("Files (Paisos a agrupar):", nrow(dades_ts_num), "\n")
cat("Columnes (Mesos temporals):", ncol(dades_ts_num), "\n")
cat("NAs:", sum(is.na(dades_ts_num)), "\n")

# COMPARATIVA VISUAL FINAL (ESCALAT VS NO ESCALAT)

cat("\n--- GENERANT COMPARATIVA VISUAL PER A L'ANNEX D ---\n")

# 1. Triem un grup de països variat (Potències vs Mitjans)
# Per exemple: Espanya (molt volum), Àustria (volum mitjà/estacional), Bèlgica (baix volum)
països_mostra <- c("Spain", "Austria", "Belgium", "Greece", "Sweden")
dades_mostra <- dades_ts_num[països_mostra, ]
dades_mostra_esc <- t(scale(t(dades_mostra))) # Escalat Z-score

par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

# --- GRÀFIC ESQUERRA: SENSE ESCALAR (Realitat de Mercat) ---
ts.plot(t(dades_mostra), 
        col = 1:5, lwd = 2, 
        main = "A. Dades Sense Escalar\n(Diferència de Volum Real)",
        ylab = "Pernoctacions totals",
        xlab = "Mesos (Sèrie Temporal)")
legend("topleft", legend = països_mostra, col = 1:5, lty = 1, cex = 0.6, bty = "n")

# --- GRÀFIC DRETA: ESCALAT Z-SCORE (Pèrdua de Volum) ---
ts.plot(t(dades_mostra_esc), 
        col = 1:5, lwd = 2, 
        main = "B. Dades Escalades (Z-score)\n(Només es compara la forma)",
        ylab = "Valor estandarditzat",
        xlab = "Mesos (Sèrie Temporal)")
# Afegim una nota visual
abline(h = 0, lty = 2, col = "gray")

cat("\nGràfic comparatiu generat. Fixa't com al Gràfic B,\n")
cat("Bèlgica i Espanya semblen 'igual d'importants'.\n")

par(mfrow = c(1, 1))

# PAS 2: COMPETICIÓ JERÀRQUICA 

dist_dtw <- proxy::dist(dades_ts_num, method = "dtw_basic")
dist_cos <- proxy::dist(dades_ts_num, method = "cosine")


hc_dtw_ward <- hclust(dist_dtw, method = "ward.D2")
hc_dtw_comp <- hclust(dist_dtw, method = "complete")
hc_dtw_avg  <- hclust(dist_dtw, method = "average")
hc_dtw_sing <- hclust(dist_dtw, method = "single")

hc_cos_ward <- hclust(dist_cos, method = "ward.D2")
hc_cos_comp <- hclust(dist_cos, method = "complete")
hc_cos_avg  <- hclust(dist_cos, method = "average")
hc_cos_sing <- hclust(dist_cos, method = "single")

taula_cofenetic <- data.frame(
  Metode_Enllac = c("Ward.D2", "Complete", "Average", "Single"),
  DTW_Cofenetic = c(
    cor(dist_dtw, cophenetic(hc_dtw_ward)),
    cor(dist_dtw, cophenetic(hc_dtw_comp)),
    cor(dist_dtw, cophenetic(hc_dtw_avg)),
    cor(dist_dtw, cophenetic(hc_dtw_sing))
  ),
  COS_Cofenetic = c(
    cor(dist_cos, cophenetic(hc_cos_ward)),
    cor(dist_cos, cophenetic(hc_cos_comp)),
    cor(dist_cos, cophenetic(hc_cos_avg)),
    cor(dist_cos, cophenetic(hc_cos_sing))
  )
)

taula_cofenetic[, 2:3] <- round(taula_cofenetic[, 2:3], 4)

print(taula_cofenetic)

par(mfrow = c(2, 4), mar = c(3, 2, 3, 1), cex.main = 1)

# Fila 1: DTW
plot(hc_dtw_ward, hang = -1, cex = 0.6, main = "1. DTW + Ward.D2")
plot(hc_dtw_comp, hang = -1, cex = 0.6, main = "2. DTW + Complete")
plot(hc_dtw_avg,  hang = -1, cex = 0.6, main = "3. DTW + Average")
plot(hc_dtw_sing, hang = -1, cex = 0.6, main = "4. DTW + Single")

# Fila 2: COSINUS
plot(hc_cos_ward, hang = -1, cex = 0.6, main = "5. COS + Ward.D2")
plot(hc_cos_comp, hang = -1, cex = 0.6, main = "6. COS + Complete")
plot(hc_cos_avg,  hang = -1, cex = 0.6, main = "7. COS + Average")
plot(hc_cos_sing, hang = -1, cex = 0.6, main = "8. COS + Single")

par(mfrow = c(1, 1)) 

# PAS 3: SELECCIÓ DE K 

dist_cos <- proxy::dist(dades_ts_num, method = "cosine")
hc_cos_ward <- hclust(dist_cos, method = "ward.D2")

# 1. CÀLCUL DE SILHOUETTE (K = 2 a 10)
sil_widths <- numeric(10)
for (k in 2:10) {
  grupos <- cutree(hc_cos_ward, k = k)
  sil_obj <- silhouette(grupos, dist_cos)
  sil_widths[k] <- mean(sil_obj[, 3])
}

# 2. CÀLCULO DEL COLZE (
alturas_codo <- rev(hc_cos_ward$height)[1:9]

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

# Gràfic 1: Silhouette 
plot(2:10, sil_widths[2:10], type = "b", pch = 19, col = "blue", lwd = 2,
     xlab = "Nombre de Clústers (K)", ylab = "Amplada Mitja Silhouette",
     main = "1. Silhouette")
abline(v = which.max(sil_widths), col = "red", lty = 2)

# Gràfic 2: El Colze
plot(2:10, alturas_codo, type = "b", pch = 19, col = "darkorange", lwd = 2,
     xlab = "Nombre de Clústers (K)", ylab = "Distancia de Fusió",
     main = "2. Mètode del Colze")

# Gràfic 3: Validació Visual 
plot(hc_cos_ward, hang = -1, cex = 0.5, 
     main = "3. Validació Visual")

mtext("Anàlisi per la Selecció de K", outer = TRUE, cex = 1.2, font = 2)

par(mfrow = c(1, 1)) 


# MODEL GUANYADOR (K = 5)

cat("\n--- MODEL FINAL (K = 5) ---\n")

dist_cos <- proxy::dist(dades_ts_num, method = "cosine")

hc_cos_ward <- hclust(dist_cos, method = "ward.D2")

# DENDROGRAMA 
dend <- as.dendrogram(hc_cos_ward)
dend <- set(dend, "branches_k_color", k = 5)

plot(dend, main = "Dendrograma Cosinus + Ward.D2 (K = 5)")
rect.dendrogram(dend, k = 5, border = 2:6, lty = 5, lwd = 2) 

hc_final_ts <- tsclust(dades_ts_num, type = "hierarchical", k = 5L, 
                       distmat = dist_cos, control = hierarchical_control(method = "ward.D2"))

cat("\n--- COMPOSICIÓ DELS 5 CLÚSTERS ---\n")
grupos_k5 <- cutree(hc_cos_ward, k = 5)

for(i in 1:5) {
  cat("\n---> CLÚSTER", i, ":\n")
  print(rownames(dades_ts_num)[grupos_k5 == i])
}



# PAS 4: K-Means(DBA) vs PAM(Cos) vs PAM(DTW)

k_final <- 5L
set.seed(123)

# 1. JERÀRQUIC (Cos + Ward)
mod_jer_cos <- tsclust(dades_ts_num, type = "hierarchical", k = k_final, 
                       distance = "cosine", control = hierarchical_control(method = "ward.D2"))

# 2. PARTICIONAL 1: K-MEANS DBA (DTW)
mod_kmeans_dba <- tsclust(dades_ts_num, type = "partitional", k = k_final, 
                          distance = "dtw_basic", centroid = "dba", trace = FALSE)

# 3. PARTICIONAL 2: PAM con Cosinus
mod_pam_cos <- tsclust(dades_ts_num, type = "partitional", k = k_final, 
                       distance = "cosine", centroid = "pam", trace = FALSE)

# 4. PARTICIONAL 3: PAM con DTW 
mod_pam_dtw <- tsclust(dades_ts_num, type = "partitional", k = k_final, 
                       distance = "dtw_basic", centroid = "pam", trace = FALSE)


taula_particional <- rbind(
  "1. JERÀRQUIC (Cosinus)" = cvi(mod_jer_cos, type = "valid"),
  "2. K-MEANS DBA (DTW)"   = cvi(mod_kmeans_dba, type = "valid"),
  "3. PAM (Coseno)"        = cvi(mod_pam_cos, type = "valid"),
  "4. PAM (DTW)"           = cvi(mod_pam_dtw, type = "valid")
)

cat("\n--- RESULTATS (CVI) ---\n")
print(round(taula_particional, 4))


# --- EVALUACIÓ VISUAL ---

p1 <- plot(mod_jer_cos, type = "sc")
p1 <- p1 + ggplot2::ggtitle("1. Jeràrquic (Cosinus + Ward)")
print(p1)

p2 <- plot(mod_kmeans_dba, type = "sc")
p2 <- p2 + ggplot2::ggtitle("2. Particional: K-Means DBA (DTW)")
print(p2)

p3 <- plot(mod_pam_cos, type = "sc")
p3 <- p3 + ggplot2::ggtitle("3. Particional: PAM (Cosinus)")
print(p3)

p4 <- plot(mod_pam_dtw, type = "sc")
p4 <- p4 + ggplot2::ggtitle("4. Particional: PAM (DTW)")
print(p4)


if(!require(gridExtra)) {
  install.packages("gridExtra")
  library(gridExtra)
}

p1 <- plot(mod_jer_cos, type = "sc") + ggplot2::ggtitle("1. Jeràrquic (Cosinus + Ward)")
p2 <- plot(mod_kmeans_dba, type = "sc") + ggplot2::ggtitle("2. K-Means DBA (DTW)")
p3 <- plot(mod_pam_cos, type = "sc") + ggplot2::ggtitle("3. PAM (Cosinus)")
p4 <- plot(mod_pam_dtw, type = "sc") + ggplot2::ggtitle("4. PAM (DTW)")

grid.arrange(p1, p2, p3, p4, ncol = 2)

cat("\n--- COMPOSICIÓ DE CLÚSTERS (K = 5) ---\n")

paises <- rownames(dades_ts_num)

clusters_jer_cos <- mod_jer_cos@cluster
clusters_kme_dba <- mod_kmeans_dba@cluster
clusters_pam_cos <- mod_pam_cos@cluster
clusters_pam_dtw <- mod_pam_dtw@cluster

imprimir_composicion <- function(nombre_modelo, vector_clusters) {
  cat("\n================================================================")
  cat("\n MODEL:", nombre_modelo)
  cat("\n================================================================\n")
  
  for(i in 1:5) {
    # Filtramos los países que pertenecen al clúster 'i'
    paises_en_cluster <- paises[vector_clusters == i]
    
    # Imprimimos el número de clúster, cuántos países tiene y sus nombres
    cat(sprintf("\n---> CLÚSTER %d (%d países):\n", i, length(paises_en_cluster)))
    print(paises_en_cluster)
  }
  cat("\n")
}

# Ejecutamos la función para nuestros 4 modelos competidores
imprimir_composicion("1. JERÀRQUIC (Cosinus + Ward.D2)", clusters_jer_cos)
imprimir_composicion("2. K-MEANS DBA (Distancia DTW)", clusters_kme_dba)
imprimir_composicion("3. PAM (Distancia Cosinus)", clusters_pam_cos)
imprimir_composicion("4. PAM (Distancia DTW)", clusters_pam_dtw)
