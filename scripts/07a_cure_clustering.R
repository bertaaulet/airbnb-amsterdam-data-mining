# ==============================================================================
# ALGORITME CURE ADAPTAT PER A DADES MIXTES (GOWER + WARD)
# ==============================================================================

# Carreguem les llibreries necessàries
library(dplyr)
library(cluster) # Per a la funció daisy()
library(gower)   # Per a la funció gower_dist() punt a punt

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓ INICIAL I PREPARACIÓ DE DADES
# ------------------------------------------------------------------------------
r <- 0.2 # Percentatge de punts a escollir com a representants (20%)
r_shrink <- 0.2 # Percentatge d'encongiment (acostar un 20% al centroide)

# Carreguem el dataset
dataset <- readRDS("../data/dataset_feature_selection_final.rds")


glimpse(dataset)

# Fem la selecció de variables per al CLUSTERING
data <- dataset %>%
  select(
    # 1. ELIMINEM VARIABLES TEXTUALS I IDENTIFICADORS (No aporten valor a la distància)
    -id, -source, -host_id, -listing_url, -picture_url, -host_url, 
    -host_thumbnail_url, -host_picture_url, -name, -description, 
    -neighborhood_overview, -host_name, -host_location, -host_about,
    -host_verifications, -amenities, -license,
    
    # 2. ELIMINEM VARIABLES GEOGRÀFIQUES PURES (Ja tenim el barri)
    -latitude, -longitude,
    
    # 3. ELIMINEM EL TARGET (Ni l'original ni la transformació logarítmica)
    -price, -log_price,
    
    # 4. ELIMINEM VARIABLES ORIGINALS QUE JA TENEN LA SEVA VERSIÓ "LOG"
    # D'aquesta manera ens quedem només amb els logaritmes per al càlcul de distàncies
    -dies_antiguitat_listing, 
    -dies_recencia_review, 
    -estimated_revenue_l365d, 
    -estimated_occupancy_l365d, 
    -minimum_nights, 
    -maximum_nights, 
    -number_of_reviews, 
    -reviews_per_month
  )

# Comprovem com queda ara 
glimpse(data)

# ------------------------------------------------------------------------------
# 1. DIVISIÓ DE LA BASE DE DADES (Mostra / No Mostra)
# ------------------------------------------------------------------------------
# Definim la mida de la mostra. IMPORTANT: Ha de ser n < 10000 per no bloquejar la RAM.
n_mostra <- ceiling(0.3 * nrow(data)) # ens quedem amb aquest valor perque representa gairebé un terç de tota la teva realitat (una qualitat altíssima) i és prou petita perquè el teu ordinador ho calculi ràpid

# Mostreig aleatori sense reemplaçament
set.seed(123) # Fixem la llavor per a la reproductibilitat
ids_mostra <- sample(1:nrow(data), size = n_mostra, replace = FALSE)

data_mostra <- data[ids_mostra, ]
data_no_mostra <- data[-ids_mostra, ]

# ------------------------------------------------------------------------------
# 2. CLUSTERING JERÀRQUIC DE LA MOSTRA (Matriu Gower^2 + Ward)
# ------------------------------------------------------------------------------
# Calculem la matriu de distàncies de Gower per a la mostra
matriu_gower <- daisy(data_mostra, metric = "gower")

# Elevem al quadrat per poder utilitzar el mètode Ward correctament amb dades mixtes
matriu_gower_sq <- matriu_gower^2

# Creem l'arbre jeràrquic utilitzant ward.D2 (que és l'estàndard a R per a Ward)
arbre <- hclust(matriu_gower_sq, method = "ward.D2")

#Dibuixar el dendrograma per prendre una decisió
# Convertim l'arbre a format 'dendrogram' per a un gràfic més net
dend_net <- as.dendrogram(arbre)

plot(dend_net, 
     leaflab = "none", # Això amaga les etiquetes
     main = "Dendrograma de la Mostra (CURE)", 
     ylab = "Alçada (Distància Gower^2)")

# Tallem l'arbre per assignar cada individu de la mostra al seu clúster inicial
k <- 3 # Suposem que observem 3 clústers al dendrograma
data_mostra$cluster <- cutree(arbre, k=k)

# ------------------------------------------------------------------------------
# 3. CÀLCUL DELS CENTROIDES MIXTOS (Mitjana + Moda)
# ------------------------------------------------------------------------------
# Creem la funció FUN = moda per a les variables categòriques
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Inicialitzem un dataframe buit per guardar els centroides
centroides <- data.frame()

for (i in 1:k) {
  # Filtrem els individus del clúster actual
  subset_cluster <- data_mostra[data_mostra$cluster == i, -ncol(data_mostra)]
  
  centroide_i <- list()
  
  # Calculem mitjana per a numèriques i moda per a categòriques
  for (col in colnames(subset_cluster)) {
    if (is.numeric(subset_cluster[[col]])) {
      centroide_i[[col]] <- mean(subset_cluster[[col]], na.rm = TRUE)
    } else {
      centroide_i[[col]] <- get_mode(subset_cluster[[col]])
    }
  }
  
  # Afegim el clúster per identificar-lo i l'unim al dataframe de centroides
  centroide_i_df <- as.data.frame(centroide_i)
  centroide_i_df$cluster_id <- i
  centroides <- bind_rows(centroides, centroide_i_df)
}
# ------------------------------------------------------------------------------
# 4. SELECCIÓ I ENCONGIMENT DE REPRESENTANTS PUNT A PUNT
# ------------------------------------------------------------------------------
representants_finals <- data.frame()

for (i in 1:k) {
  # Agafem els individus d'aquest clúster i el seu centroide original
  subset_cluster <- data_mostra[data_mostra$cluster == i, -ncol(data_mostra)]
  cent_actual <- centroides[centroides$cluster_id == i, -ncol(centroides)]
  
  # Calculem la distància de Gower de cada punt contra el centroide (un a un)
  distancies_al_centroide <- gower_dist(cent_actual, subset_cluster)
  
  # Ordenem per quedar-nos amb els més allunyats (el 20%)
  n_rep <- max(1, floor(r * nrow(subset_cluster)))
  indexos_ordenats <- order(distancies_al_centroide, decreasing = TRUE)
  indexos_reps <- indexos_ordenats[1:n_rep]
  
  reps_originals <- subset_cluster[indexos_reps, ]
  
  # ENCONGIMENT (Shrinking) del 20% cap al centroide
  reps_encongits <- reps_originals
  
  for (col in colnames(reps_encongits)) {
    if (is.numeric(reps_encongits[[col]])) {
      # Matemàtica: Valor_Nou = Valor_Vell + 0.20 * (Valor_Centroide - Valor_Vell)
      reps_encongits[[col]] <- reps_originals[[col]] + 
        r_shrink * (cent_actual[[col]] - reps_originals[[col]])
    }
    # Les variables categòriques es queden igual, conservant la seva categoria
  }
  
  # Etiquetem a quin clúster pertanyen aquests representants encongits
  reps_encongits$cluster_rep <- i
  representants_finals <- bind_rows(representants_finals, reps_encongits)
}

# ------------------------------------------------------------------------------
# 5. RECALCULAR CENTROIDES* (Amb representants encongits i centroide original)
# ------------------------------------------------------------------------------
# Segons els apunts: "recalcular centroide con los representantes y el centroide original"

nous_centroides <- data.frame()

for (i in 1:k) {
  # Agafem els representants encongits d'aquest clúster
  reps_cluster_i <- representants_finals[representants_finals$cluster_rep == i, -ncol(representants_finals)]
  
  # Agafem el centroide original d'aquest clúster
  cent_actual <- centroides[centroides$cluster_id == i, -ncol(centroides)]
  
  # Unim els representants i el centroide original per calcular el "centroide*"
  dades_per_nou_centroide <- bind_rows(reps_cluster_i, cent_actual)
  
  nou_centroide_i <- list()
  
  # Recalculem el centroide definitiu (mitjana per numèriques, moda per categòriques)
  for (col in colnames(dades_per_nou_centroide)) {
    if (is.numeric(dades_per_nou_centroide[[col]])) {
      nou_centroide_i[[col]] <- mean(dades_per_nou_centroide[[col]], na.rm = TRUE)
    } else {
      nou_centroide_i[[col]] <- get_mode(dades_per_nou_centroide[[col]])
    }
  }
  
  # Ho guardem al dataframe final de Nous Centroides
  nou_centroide_df <- as.data.frame(nou_centroide_i)
  nou_centroide_df$cluster_id <- i
  nous_centroides <- bind_rows(nous_centroides, nou_centroide_df)
}

# ------------------------------------------------------------------------------
# 6. ASSIGNACIÓ DE LA NO_MOSTRA AL CENTROIDE* MÉS PROPER
# ------------------------------------------------------------------------------
# Segons els apunts: "etiquetandolos al centroide que le quede mas cerca (menor distancia)"
# Aquest pas assignarà els punts restants segons la mètrica de Gower

cluster_assignat <- numeric(nrow(data_no_mostra))

# Dades dels nous centroides sense la columna de l'ID per poder calcular Gower net
dades_nous_centroides <- nous_centroides[, -ncol(nous_centroides)]

for (i in 1:nrow(data_no_mostra)) {
  # Agafem un individu de la no mostra
  individu <- data_no_mostra[i, ]
  
  # Calculem la distància de Gower contra els k NOUS CENTROIDES* (ja no contra els representants)
  distancies_nous_cent <- gower_dist(individu, dades_nous_centroides)
  
  # Busquem quin és el centroide* més proper (distància mínima)
  index_min <- which.min(distancies_nous_cent)
  
  # Li assignem l'ID del clúster corresponent
  cluster_assignat[i] <- nous_centroides$cluster_id[index_min]
}

# Afegim l'etiqueta final al dataframe de la no mostra
data_no_mostra$cluster <- cluster_assignat


# ------------------------------------------------------------------------------
# 7. RESULTATS I PERFILAT
# ------------------------------------------------------------------------------
# 1. Unim tota la base de dades amb les seves etiquetes de clúster
df_clusters <- bind_rows(data_mostra, data_no_mostra)

# 2. Afegim la columna 'id' al dataframe de clústers per poder creuar.
# Com que rownames conté el número de fila original, podem buscar el 'id' al 'dataset'
df_clusters$id <- dataset$id[as.numeric(rownames(df_clusters))]

# 3. CREUAMENT SEGUR: Ajuntem les etiquetes de clúster amb TODA la base de dades original
# Això et permetrà perfilar amb el preu real, habitacions reals, etc.
df_final <- dataset %>%
  left_join(select(df_clusters, id, cluster), by = "id")

# Ara ja pots fer el perfilat final de tot el dataset junt
print(table(df_final$cluster))


# ------------------------------------------------------------------------------
# 8. AVALUACIÓ NUMÈRICA DEL MODEL (TOTS ELS REGISTRES)
# ------------------------------------------------------------------------------
cat("\n--- AVALUACIÓ NUMÈRICA DEL MODEL (100% DE LES DADES) ---\n")

# --- 1. CRITERI MATEMÀTIC: SILHOUETTE SCORE GLOBAL ---
cat("Calculant matriu de Gower per a tota la base de dades... (Això pot trigar un parell de minuts)\n")

# Apliquem EXACTAMENT la mateixa neteja directament sobre df_final
dades_netes_R_global <- df_final %>% 
  select(
    -cluster, # Traiem la variable que acabem de crear
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
  )

# Calculem la matriu Gower d'absolutament tots els registres
dist_eval_R_global <- daisy(dades_netes_R_global, metric = "gower")

cat("Matriu calculada! Extraient el Silhouette Score...\n")
# Calculem la Silueta comparant cada pis amb els altres 10.452
sil_R_global <- silhouette(df_final$cluster, dist_eval_R_global)
mitjana_silueta_R_global <- mean(sil_R_global[, 3])

cat("\n1. Silhouette Score GLOBAL (Cohesió/Separació):", round(mitjana_silueta_R_global, 4), "\n")


# --- 2. CRITERI DE NEGOCI: VARIANÇA INTER-CLÚSTER ---
# Calculem la mitjana d'ingressos i preus de cada clúster (ja es feia sobre tot el dataset)
mitjanes_cluster <- df_final %>%
  group_by(cluster) %>%
  summarise(
    ingressos_mitjans = mean(estimated_revenue_l365d, na.rm = TRUE),
    preu_mitja = mean(price, na.rm = TRUE)
  )

# Calculem com de separats estan aquests grups entre ells (variança)
var_ingressos <- var(mitjanes_cluster$ingressos_mitjans)
var_preus <- var(mitjanes_cluster$preu_mitja)

cat("2. Variança d'Ingressos estimats entre clústers:", round(var_ingressos, 2), "\n")
cat("3. Variança de Preus entre clústers:", round(var_preus, 2), "\n")

# Opcional: Imprimim el resum de negoci per tenir-ho a mà
cat("\n--- PERFILAT RÀPID DE NEGOCI ---\n")
print(as.data.frame(mitjanes_cluster))


saveRDS(df_final, file = "../data/dataset_cure_R.rds")

cat("Arxiu RDS guardat correctament!\n")
