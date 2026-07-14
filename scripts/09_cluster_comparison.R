# ==============================================================================
# SCRIPT DE COMPARATIVA DE MODELS DE CLUSTERING (CURE k=3 vs K-PROTO k=3)
# ==============================================================================

# Carreguem les llibreries necessàries
library(dplyr)
library(cluster)
library(flexclust) # Necessària per a randIndex()
# install.packages("flexclust") # Descomentar si no la tens instal·lada

# ------------------------------------------------------------------------------
# 1. CARREGAR I UNIFICAR ELS DATASETS
# ------------------------------------------------------------------------------
cat("1. Carregant i unificant les dades...\n")

df_cure <- readRDS("../data/dataset_cure_python.rds") 
df_kproto <- readRDS("../data/dataset_kproto.rds") 

# CORRECCIÓ DEFINITIVA PER ALS IDs:
df_cure <- df_cure %>% mutate(id = trimws(format(id, scientific = FALSE)))
df_kproto <- df_kproto %>% mutate(id = trimws(format(id, scientific = FALSE)))

df_cure <- df_cure %>% mutate(id = gsub("\\.0+$", "", id))
df_kproto <- df_kproto %>% mutate(id = gsub("\\.0+$", "", id))

# Ens assegurem que les columnes de clúster tinguin noms diferents i unificem per 'id'
df_kproto_net <- df_kproto %>% 
  select(id, cluster_kproto = cluster)

df_comparativa <- df_cure %>%
  rename(cluster_cure = cluster) %>%
  left_join(df_kproto_net, by = "id")

# Comprovació de seguretat
cat("Dades unificades correctament. Total registres:", nrow(df_comparativa), "\n")
nuls <- sum(is.na(df_comparativa$cluster_kproto))

if(nuls > 0) {
  cat("ATENCIÓ: Hi ha", nuls, "valors Nuls a l'encreuament!\n")
} else {
  cat("L'encreuament per ID ha estat perfecte (0 Nuls).\n\n")
}

# ------------------------------------------------------------------------------
# 2. PREPARACIÓ DE DADES PER A LA SILUETA (AMB MOSTRA PER RAM)
# ------------------------------------------------------------------------------
cat("2. Preparant la matriu de distàncies de Gower...\n")

set.seed(123)
mostra_eval <- df_comparativa %>% sample_n(min(2000, nrow(df_comparativa)))

# Netegem traient identificadors i metadades, i ajustem els tipus de dades
dades_netes <- mostra_eval %>% 
  select(
    -cluster_cure, -cluster_kproto,
    -id, -source, -host_id, -listing_url, -picture_url, -host_url, 
    -host_thumbnail_url, -host_picture_url, -name, -description, 
    -neighborhood_overview, -host_name, -host_location, -host_about,
    -host_verifications, -amenities, -license,
    -latitude, -longitude,
    -price, -log_price,
    -dies_antiguitat_listing, -dies_recencia_review, 
    -estimated_revenue_l365d, -estimated_occupancy_l365d, 
    -minimum_nights, -maximum_nights, 
    -number_of_reviews, -reviews_per_month
  ) %>%
  # 1. Convertim tot el text a factor
  mutate_if(is.character, as.factor) %>%
  # 2. Convertim les variables lògiques (TRUE/FALSE) a factor
  mutate_if(is.logical, as.factor) %>%
  # 3. Forcem les variables binàries numèriques a factor (com al dataset original)
  mutate(
    listing_is_new = as.factor(listing_is_new),
    bath_is_shared = as.factor(bath_is_shared)
  )

# Calculem la matriu Gower
dist_eval <- daisy(dades_netes, metric = "gower")

# ------------------------------------------------------------------------------
# 3. CÀLCUL DEL SILHOUETTE SCORE
# ------------------------------------------------------------------------------
cat("\n--- 3. RESULTATS DEL SILHOUETTE SCORE ---\n")

sil_cure <- silhouette(mostra_eval$cluster_cure, dist_eval)
sil_kproto <- silhouette(mostra_eval$cluster_kproto, dist_eval)

cat("[GLOBAL] Silhouette CURE (k=3):", round(mean(sil_cure[, 3]), 4), "\n")
cat("[GLOBAL] Silhouette K-Proto (k=3):", round(mean(sil_kproto[, 3]), 4), "\n\n")

# ------------------------------------------------------------------------------
# 4. AVALUACIÓ DE SIMILITUD ENTRE MODELS (RAND INDEX I MATRIU)
# ------------------------------------------------------------------------------
cat("\n--- 4. COMPARATIVA DIRECTA ENTRE CURE I K-PROTO (k=3) ---\n")

# A) Matriu de coincidència
taula_coincidencia <- table(CURE = df_comparativa$cluster_cure, 
                            KProto = df_comparativa$cluster_kproto)
cat("\nMATRIU DE COINCIDÈNCIA (CURE en files, K-Proto en columnes):\n")
print(taula_coincidencia)

# B) Rand Index
# Mesura la proporció d'acords entre els dos agrupaments (1 = idèntics, 0 = atzar)
ri_cure_kproto <- randIndex(taula_coincidencia)
cat("\nÍNDEX DE RAND (Similitud entre els dos models):", round(ri_cure_kproto, 4), "\n")

# C) Visualització: Mosaic Plot
# Generem un gràfic per veure visualment com s'encavalquen els clústers
mosaicplot(taula_coincidencia, shade = TRUE, las = 1,
           main = "Intersecció de Clústers: CURE vs K-Prototypes",
           xlab = "Clústers CURE", ylab = "Clústers K-Prototypes")


# ------------------------------------------------------------------------------
# 5. CÀLCUL DE LA VARIANÇA DE NEGOCI (AMPLIAT A 4 KPIs CLAU)
# ------------------------------------------------------------------------------
cat("\n--- 5. RESULTATS DE NEGOCI (VARIANÇA INTER-CLÚSTER) ---\n")

# Funcions per calcular centroides de negoci
mitjanes_cure <- df_comparativa %>%
  group_by(cluster_cure) %>%
  summarise(
    ingressos_m = mean(estimated_revenue_l365d, na.rm = TRUE),
    preu_m      = mean(price, na.rm = TRUE),
    ocupacio_m  = mean(estimated_occupancy_l365d, na.rm = TRUE),
    reviews_m   = mean(reviews_per_month, na.rm = TRUE)
  )

mitjanes_kproto <- df_comparativa %>%
  group_by(cluster_kproto) %>%
  summarise(
    ingressos_m = mean(estimated_revenue_l365d, na.rm = TRUE),
    preu_m      = mean(price, na.rm = TRUE),
    ocupacio_m  = mean(estimated_occupancy_l365d, na.rm = TRUE),
    reviews_m   = mean(reviews_per_month, na.rm = TRUE)
  )

# Imprimim la comparativa de variances globals
cat("1. VARIANÇA D'INGRESSOS\n")
cat("   CURE (k=3):", format(var(mitjanes_cure$ingressos_m), scientific=FALSE, big.mark=","), "\n")
cat("   K-PR (k=3):", format(var(mitjanes_kproto$ingressos_m), scientific=FALSE, big.mark=","), "\n\n")

cat("2. VARIANÇA DE PREUS\n")
cat("   CURE (k=3):", format(var(mitjanes_cure$preu_m), scientific=FALSE, big.mark=","), "\n")
cat("   K-PR (k=3):", format(var(mitjanes_kproto$preu_m), scientific=FALSE, big.mark=","), "\n\n")

cat("3. VARIANÇA D'OCUPACIÓ ANUAL\n")
cat("   CURE (k=3):", round(var(mitjanes_cure$ocupacio_m), 2), "\n")
cat("   K-PR (k=3):", round(var(mitjanes_kproto$ocupacio_m), 2), "\n\n")

# ------------------------------------------------------------------------------
# 6. CENTROIDES DE NEGOCI (TAULES RESUM)
# ------------------------------------------------------------------------------
cat("\n--- 6. CENTROIDES CURE (k=3) ---\n")
print(as.data.frame(mitjanes_cure))

cat("\n--- 6. CENTROIDES K-PROTO (k=3) ---\n")
print(as.data.frame(mitjanes_kproto))

print(table(df_comparativa$cluster_cure))
print(table(df_comparativa$cluster_kproto))
