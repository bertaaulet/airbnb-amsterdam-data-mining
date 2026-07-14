library(EnvStats)
library(outliers)
library(ggplot2)

dades <- readRDS("../data/dataset_imputat.rds")

# Busquem quines variables són numèriques (integer o numeric)
es_numeric <- sapply(dades, is.numeric)
varNum <- names(dades)[es_numeric]

# Excloem les variables que no tenen sentit per a la detecció d'outliers
vars_a_excloure <- c("id", "host_id", "latitude", "longitude")

varNum_net <- varNum[!(varNum %in% vars_a_excloure)]

# Creem el sub-dataset només amb el que ens interessa analitzar
dades_num <- dades[, varNum_net]

# ==============================================================================
# FASE 1: DETECCIÓ UNIVARIANT
# ==============================================================================

aplicar_univariants <- function(nom_variable, df) {
  cat("\n================================================\n")
  cat("Analitzant variable:", nom_variable, "\n")
  cat("================================================\n")
  
  x <- df[[nom_variable]]
  
  x <- na.omit(x) 
  
  if(length(x) < 20) {
    cat("No hi ha prou dades per analitzar.\n")
    return(NULL)
  }
  
  # 1. Mínims i Màxims (Molt útil per veure coses rares, ex: price = 0)
  cat("[1] Mínim:", min(x), "| Màxim:", max(x), "\n")
  
  # 2. IQR (Mètode de Tukey / Boxplot)
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- Q3 - Q1
  idx_iqr <- which(x < (Q1 - 1.5 * IQR_val) | x > (Q3 + 1.5 * IQR_val))
  cat("[2] Outliers per IQR:", length(idx_iqr), "observacions.\n")
  
  # 3. Z-Score (Valors a més de 3 desviacions estàndard)
  if(sd(x) > 0) {
    z_scores <- scale(x)
    idx_z <- which(abs(z_scores) > 3)
    cat("[3] Outliers per Z-Score (>3 SD):", length(idx_z), "observacions.\n")
  } else {
    cat("[3] Z-Score no calculable (variància zero).\n")
  }
}

for(var in colnames(dades_num)) {
  aplicar_univariants(var, dades_num)
}

# ==============================================================================
# FASE 2: DETECCIÓ MULTIVARIANT
# ==============================================================================

paquets_multi <- c("chemometrics", "dbscan", "isotree", "tclust", "caret", "adamethods", "randomForest")
nous <- paquets_multi[!(paquets_multi %in% installed.packages()[,"Package"])]
if(length(nous) > 0) install.packages(nous)

library(chemometrics)
library(dbscan)
library(isotree)
library(tclust)
library(caret)
library(randomForest)

cat("\n================================================\n")
cat("PAS 2.1: FILTRATGE INTEL·LIGENT (EVITAR COL·LINEALITAT)\n")
cat("================================================\n")

# A. Eliminar variables amb variància zero o gairebé zero (MAD=0)
nzv_idx <- nearZeroVar(dades_num, saveMetrics = FALSE)
if(length(nzv_idx) > 0) {
  dades_netes <- dades_num[, -nzv_idx]
  cat("- Eliminades", length(nzv_idx), "variables per variància gairebé nul·la.\n")
} else {
  dades_netes <- dades_num
}

# B. Eliminar variables altament correlacionades (ex: minimum_nights vs minimum_minimum_nights)
matriu_corr <- cor(dades_netes, use = "pairwise.complete.obs")
altament_corr <- findCorrelation(matriu_corr, cutoff = 0.85) # Llindar del 85%
if(length(altament_corr) > 0) {
  dades_netes <- dades_netes[, -altament_corr]
  cat("- Eliminades", length(altament_corr), "variables per alta correlació (col·linealitat).\n")
}

# C. Eliminar combinacions lineals exactes (El pas clau per a Mahalanobis!)
combos <- findLinearCombos(dades_netes)
if(!is.null(combos$remove)) {
  dades_netes <- dades_netes[, -combos$remove]
  cat("- Eliminades", length(combos$remove), "variables per combinació lineal exacta.\n")
} else {
  cat("- No s'han trobat combinacions lineals exactes.\n")
}

cat("- Ens quedem amb", ncol(dades_netes), "variables numèriques per als algorismes.\n")

# Escalem les dades netes per als mètodes de distàncies
dades_scaled <- scale(dades_netes)

cat("\n================================================\n")
cat("PAS 2.2: APLICACIÓ DE TOTS ELS MÈTODES MULTIVARIANTS\n")
cat("================================================\n")

library(isotree)
library(dbscan)
library(chemometrics)
library(tclust)
library(randomForest)

# 1. Isolation Forest
set.seed(123) 
iso_model <- isolation.forest(dades_netes, ntrees = 100)
pred_iso <- predict(iso_model, dades_netes)
outliers_if <- which(pred_iso > 0.60)
cat("[1] Isolation Forest (>0.60):", length(outliers_if), "outliers.\n")

# 2. Local Outlier Factor (LOF)
lof_scores <- lof(dades_scaled, minPts = 10)
outliers_lof <- which(lof_scores > 2) 
cat("[2] LOF (>2):", length(outliers_lof), "outliers.\n")

# 3. PCA (Error de Reconstrucció)
pca_model <- prcomp(dades_netes, center = TRUE, scale. = TRUE)
k_comp <- min(5, ncol(dades_netes)) 
recon <- pca_model$x[, 1:k_comp] %*% t(pca_model$rotation[, 1:k_comp])
recon <- scale(recon, center = FALSE, scale = 1/pca_model$scale) 
recon <- scale(recon, center = -pca_model$center, scale = FALSE) 
error_recon <- rowMeans((dades_netes - recon)^2)
outliers_pca <- which(error_recon > quantile(error_recon, 0.99))
cat("[3] PCA (Top 1% error):", length(outliers_pca), "outliers.\n")

# 4. Mahalanobis Clàssic
tryCatch({
  dist_mah <- mahalanobis(dades_netes, colMeans(dades_netes), cov(dades_netes))
  outliers_mah <- which(dist_mah > qchisq(p = 0.99, df = ncol(dades_netes)))
  cat("[4] Mahalanobis Clàssic:", length(outliers_mah), "outliers.\n")
}, error = function(e) {
  cat("[4] Mahalanobis Clàssic: Descartat (Matriu Singular).\n")
  outliers_mah <- integer(0) # Retornem buit si falla
})

# 5. Mahalanobis Robust (Moutlier)
tryCatch({
  # Captura els missatges de la consola per evitar que 'Moutlier' ompli la pantalla
  invisible(capture.output(res_moutlier <- chemometrics::Moutlier(dades_netes, quantile = 0.99, plot = FALSE)))
  outliers_mah_rob <- which(res_moutlier$rd > res_moutlier$cutoff)
  cat("[5] Mahalanobis Robust:", length(outliers_mah_rob), "outliers.\n")
}, error = function(e) {
  cat("[5] Mahalanobis Robust: Descartat (Matriu Singular).\n")
  outliers_mah_rob <- integer(0)
})

# FALLA MAHALANOBIS (CLÀSSIC I ROBUST)

# Tot i haver aplicat els filtres previs (variància zero i col·linealitat), 
# aquests dos algorismes fallen donant l'error de "Matriu Singular".
# 
# El motiu matemàtic és la topologia de les dades d'Airbnb: tenim milers de 
# pisos amb característiques idèntiques (per exemple, mateix nombre de llits, 
# banys, i puntuacions similars). Això provoca que milers d'observacions 
# estiguin al mateix "hiperplà" exacte.
#
# Com a conseqüència, la matriu de covariància resultant té un determinant 
# igual (o gairebé igual) a zero, fent-la matemàticament no invertible. 
# Atès que la fórmula de Mahalanobis exigeix invertir aquesta matriu, 
# l'algorisme col·lapsa.

cat("\n================================================\n")
cat(" MAHALANOBIS AMB FILTRATGE ESTRICTE\n")
cat("================================================\n")

# 1. Detectem quines variables tenen més del 50% de valors idèntics
n_rows <- nrow(dades_netes)
vars_problematiques <- c()

for(col in colnames(dades_netes)) {
  # Comptem quantes vegades es repeteix el valor més freqüent
  max_freq <- max(table(dades_netes[[col]]))
  
  if(max_freq / n_rows > 0.50) { 
    vars_problematiques <- c(vars_problematiques, col)
  }
}

cat("- Variables que fan petar Mahalanobis (>50% repetides):", 
    paste(vars_problematiques, collapse=", "), "\n")

# 2. Creem un dataset exclusiu per a Mahalanobis sense aquestes variables
dades_mah <- dades_netes[, !(colnames(dades_netes) %in% vars_problematiques)]
cat("- Calculem Mahalanobis amb", ncol(dades_mah), "variables 100% segures.\n")

# 3. Llancem Mahalanobis Clàssic
tryCatch({
  dist_mah <- mahalanobis(dades_mah, colMeans(dades_mah), cov(dades_mah))
  outliers_mah <- which(dist_mah > qchisq(p = 0.99, df = ncol(dades_mah)))
  cat("\n[4] Mahalanobis Clàssic:", length(outliers_mah), "outliers.\n")
}, error = function(e) cat("[4] Mahalanobis Clàssic segueix fallant.\n"))

# 4. Llancem Mahalanobis Robust (Moutlier)
tryCatch({
  invisible(capture.output(res_moutlier <- chemometrics::Moutlier(dades_mah, quantile = 0.99, plot = FALSE)))
  outliers_mah_rob <- which(res_moutlier$rd > res_moutlier$cutoff)
  cat("[5] Mahalanobis Robust:", length(outliers_mah_rob), "outliers.\n")
}, error = function(e) cat("[5] Mahalanobis Robust segueix fallant.\n"))

# L'algorisme de Mahalanobis Robust (Minimum Covariance Determinant - MCD) 
# necessita que almenys el 50% de les dades no siguin idèntiques per poder 
# calcular el determinant de la matriu. Com que al nostre dataset d'Airbnb 
# hi ha moltes columnes on la majoria de pisos tenen el mateix valor, intentem 
# primer una solució agressiva: aïllar temporalment qualsevol variable que 
# tingui més d'un 50% de valors repetits.
# 
# Tot i aplicar aquest filtre i quedar-nos només amb 9 variables, 
# el mètode robust seguirà fallant. L'algorisme MCD ens avisarà que, fins i tot
# amb aquestes 9 variables, la covariància esdevé singular perquè milers de 
# pisos formen un hiperplà perfecte (equacions lineals exactes). 

cat("\n================================================\n")
cat("PAS PCA I JUSTIFICACIÓ DE COMPONENTS PER A MAHALANOBIS\n")
cat("================================================\n")

# 1. Calculem el PCA
pca_model <- prcomp(dades_netes, center = TRUE, scale. = TRUE)

# 2. Extraiem la variància acumulada (El que demana en Dante per justificar)
var_acumulada <- summary(pca_model)$importance[3, ]
cat("Variància acumulada pels primers components:\n")
print(round(var_acumulada[1:min(15, length(var_acumulada))], 4))

# 3. Busquem quants components necessitem per explicar el 80% de les dades
k_optimo <- min(which(var_acumulada >= 0.80))
cat("\n-> DECISIÓ (Fent cas a l'estudi de variància): Seleccionem", k_optimo, 
    "components perquè retenen almenys el 80% de la informació original.\n")

# 4. Creem el nou dataset només amb aquests components
dades_pca_mah <- pca_model$x[, 1:k_optimo]

cat("\n================================================\n")
cat("PAS MAHALANOBIS SOBRE L'ESPAI PCA (", k_optimo, "components )\n")
cat("================================================\n")

# A. Mahalanobis Clàssic
tryCatch({
  dist_mah_pca <- mahalanobis(dades_pca_mah, colMeans(dades_pca_mah), cov(dades_pca_mah))
  outliers_mah_pca <- which(dist_mah_pca > qchisq(p = 0.99, df = ncol(dades_pca_mah)))
  cat("[3] Mahalanobis Clàssic (via PCA):", length(outliers_mah_pca), "outliers.\n")
}, error = function(e) cat("[3] Mahalanobis Clàssic segueix fallant.\n"))

# B. Mahalanobis Robust (Moutlier)
tryCatch({
  invisible(capture.output(res_moutlier_pca <- chemometrics::Moutlier(dades_pca_mah, quantile = 0.99, plot = FALSE)))
  outliers_mah_rob_pca <- which(res_moutlier_pca$rd > res_moutlier_pca$cutoff)
  cat("[4] Mahalanobis Robust (via PCA):", length(outliers_mah_rob_pca), "outliers.\n")
}, error = function(e) cat("[4] Mahalanobis Robust segueix fallant.\n"))

cat("\n================================================\n")
cat("PAS ESTUDI DE COMPONENTS PER A MAHALANOBIS (PROVES EMPÍRIQUES)\n")
cat("================================================\n")

# ==============================================================================
# Inicialment hem provat de retenir el 80% de la variància (k = 11 components). 
# Resultat: Mahalanobis Clàssic detecta ~400 outliers, però el Robust (MCD) 
# es dispara a ~4.952 outliers (gairebé la meitat del dataset!).
# Això passa per la "maledicció de la dimensionalitat": al tenir tantes 
# dimensions, l'algorisme robust es torna massa sensible al soroll de les dades.
# Seguint les indicacions de classe, farem proves reduint el nombre de components
# per veure on s'estabilitza el mètode robust.
# ==============================================================================

cat("\n--- PROVA 1: RETENINT EL 60% DE LA VARIÀNCIA ---\n")
k_60 <- min(which(var_acumulada >= 0.60))
cat("Components necessaris per al 60% de variància:", k_60, "\n")

dades_pca_60 <- pca_model$x[, 1:k_60]

# Mahalanobis Clàssic (60%)
tryCatch({
  dist_mah_60 <- mahalanobis(dades_pca_60, colMeans(dades_pca_60), cov(dades_pca_60))
  outliers_mah_60 <- which(dist_mah_60 > qchisq(p = 0.99, df = ncol(dades_pca_60)))
  cat("-> Mahalanobis Clàssic (", k_60, "comp):", length(outliers_mah_60), "outliers.\n")
}, error = function(e) cat("-> Mahalanobis Clàssic falla.\n"))

# Mahalanobis Robust (60%)
tryCatch({
  invisible(capture.output(res_rob_60 <- chemometrics::Moutlier(dades_pca_60, quantile = 0.99, plot = FALSE)))
  outliers_rob_60 <- which(res_rob_60$rd > res_rob_60$cutoff)
  cat("-> Mahalanobis Robust (", k_60, "comp):", length(outliers_rob_60), "outliers.\n")
}, error = function(e) cat("-> Mahalanobis Robust falla.\n"))


cat("\n--- PROVA 2: FIXANT 5 COMPONENTS EMPÍRICAMENT ---\n")
k_5 <- 5
cat("Variància retinguda amb 5 components:", round(var_acumulada[5] * 100, 2), "%\n")

dades_pca_5 <- pca_model$x[, 1:k_5]

# Mahalanobis Clàssic (5 comp)
tryCatch({
  dist_mah_5 <- mahalanobis(dades_pca_5, colMeans(dades_pca_5), cov(dades_pca_5))
  outliers_mah_5 <- which(dist_mah_5 > qchisq(p = 0.99, df = ncol(dades_pca_5)))
  cat("-> Mahalanobis Clàssic (5 comp):", length(outliers_mah_5), "outliers.\n")
}, error = function(e) cat("-> Mahalanobis Clàssic falla.\n"))

# Mahalanobis Robust (5 comp)
tryCatch({
  invisible(capture.output(res_rob_5 <- chemometrics::Moutlier(dades_pca_5, quantile = 0.99, plot = FALSE)))
  outliers_rob_5 <- which(res_rob_5$rd > res_rob_5$cutoff)
  cat("-> Mahalanobis Robust (5 comp):", length(outliers_rob_5), "outliers.\n")
}, error = function(e) cat("-> Mahalanobis Robust falla.\n"))

# ==============================================================================
# CONCLUSIÓ FINAL DE L'ESTUDI PCA I MAHALANOBIS:
# ==============================================================================
# 1. Respecte a les proves de variància: Hem comprovat que reduir dràsticament
#    els components (com a la prova de k=5) per intentar estabilitzar el mètode 
#    Robust és contraproduent. A més de perdre gairebé la meitat de la informació 
#    original, NO millorem els resultats (el Robust segueix marcant ~4.500 outliers).
#    Per tant, fixem el tall òptim en k=6 components (60% de variància mínima viable).
# 
# 2. Respecte al Mahalanobis Clàssic: Es comporta de forma estable i coherent 
#    amb la realitat de les dades, detectant ~328 outliers. Aquest resultat SÍ 
#    s'inclourà a la Matriu de Consens.
#
# 3. Respecte al Mahalanobis Robust (MCD): Es DESCARTA definitivament.
#    Com hem demostrat empíricament, la topologia hiper-densa d'Airbnb (milers 
#    de pisos idèntics) fa que el MCD marqui com a anomalia gairebé la meitat 
#    del dataset, independentment de la reducció de dimensionalitat que apliquem.
# ==============================================================================

cat("\n================================================\n")
cat("PAS 2.3: DBSCAN PER A OUTLIERS\n")
cat("================================================\n")

library(dbscan)

# 1. Definim els paràmetres
# minPts acostuma a ser el nombre de variables + 1, (tenim 19 variables)
min_punts <- 20 

# Per trobar la 'eps' ideal, mirem el gràfic kNNdistplot. 
# L'eps ideal és on la corba "trenca" o puja de cop.
kNNdistplot(dades_scaled, k = min_punts)
abline(h = 4, col = "red", lty = 2) # Dibuixem una línia d'ajuda

# El colze del nostre gràfic està al voltant de 4
eps_escollit <- 4.0 

# 2. Executem DBSCAN
res_dbscan <- dbscan(dades_scaled, eps = eps_escollit, minPts = min_punts)

# 3. Extraiem els outliers (els que cauen al clúster 0)
outliers_dbscan <- which(res_dbscan$cluster == 0)

cat("[10] DBSCAN (eps =", eps_escollit, ", minPts =", min_punts, "):", length(outliers_dbscan), "outliers (Soroll).\n")

cat("\n================================================\n")
cat("PAS 3: MATRIU DE CONSENS I EXTRACCIÓ DE SOSPITOSOS\n")
cat("================================================\n")

# 1. Creem un vector amb tants zeros com files té el nostre dataset net
vots_outliers <- rep(0, nrow(dades_netes))

# 2. Sumem 1 vot per cada vegada que una fila apareix a les llistes validades
vots_outliers[outliers_if] <- vots_outliers[outliers_if] + 1
vots_outliers[outliers_lof] <- vots_outliers[outliers_lof] + 1
vots_outliers[outliers_pca] <- vots_outliers[outliers_pca] + 1
vots_outliers[outliers_mah_60] <- vots_outliers[outliers_mah_60] + 1  # Usem el de 6 components!
vots_outliers[outliers_dbscan] <- vots_outliers[outliers_dbscan] + 1

# 3. Creem un resum de les votacions
taula_vots <- table(vots_outliers)
cat("\n--- RECOMPTE DE VOTS (Sobre 5 mètodes) ---\n")
print(taula_vots)

# 4. Definim els sospitosos (Llindar: >= 3 vots)
# Com que ara tenim menys mètodes, 3 vots ja és una forta indicació d'anomalia.
llindar_vots <- 3 
outliers_sospitosos <- which(vots_outliers >= llindar_vots)

cat("\n================================================\n")
cat("CONCLUSIÓ: Hem trobat", length(outliers_sospitosos), "sospitosos (amb >=", llindar_vots, "vots).\n")
cat("================================================\n")

# 5. AÏLLAR ELS INDIVIDUS PER A LA INSPECCIÓ MANUAL (El que demana Dante)
# Creem un dataframe només amb les files d'aquests pisos sospitosos
taula_revisio_manual <- dades[outliers_sospitosos, ]

# Ordenem la taula per preu (de més car a més barat) per facilitar la revisió visual
if("price" %in% colnames(taula_revisio_manual)) {
  taula_revisio_manual <- taula_revisio_manual[order(-taula_revisio_manual$price), ]
}

# 6. Exportem a CSV per poder obrir-ho a Excel i debatre en grup
write.csv(taula_revisio_manual, file = "../data/pisos_sospitosos_per_revisar.csv", row.names = FALSE)
cat("- S'ha creat el fitxer 'pisos_sospitosos_per_revisar.csv' amb èxit!\n")



# ==============================================================================
# FASE 4: NETEJA FINAL - IMPUTACIÓ DIFERENCIADA PER VARIABLE
# ==============================================================================
# Estratègia consensuada amb l'equip de missings:
#   - price (x2)       -> MICE (Random Forest)  [igual que missings originals]
#   - bathrooms        -> KNN                    [igual que missings originals]
#   - minimum_nights   -> MICE + KNN (comparem) [nou cas, decidim el millor]
# ==============================================================================

library(mice)
library(VIM)  # Per a KNN

# --- PAS 0: Còpia de treball ---
dades_working <- dades

# ==============================================================================
# 1. TRIMMING
# ==============================================================================
ids_a_eliminar <- c(
  "915003385268939136", "1046227077999613056", "1046227413759630336",
  "1343478555180632832", "949168651353554048", "1405075403645078784",
  "1442065246432693504", "1446767801912703232", "1446767877991491840",
  "1446767893271682816", "27710965", "1503867342263201536", "18882385", "18816109"
)
n_abans <- nrow(dades_working)
dades_working <- dades_working[!(dades_working$id %in% ids_a_eliminar), ]
cat("- TRIMMING: Eliminats", n_abans - nrow(dades_working), "registres (esperats: 14).\n")

# ==============================================================================
# 2. DATA RECTIFICATION: Waterwolf
# ==============================================================================
dades_working[dades_working$id == "29874862", "room_type"] <- "Entire home/apt"
cat("- RECTIFICACIÓ: Waterwolf -> room_type = 'Entire home/apt'.\n")

# ==============================================================================
# 3. CONVERSIÓ A NA DELS 4 OUTLIERS
# ==============================================================================
dades_working[dades_working$id == "1325166309003730944", "price"]          <- NA  # 9.999€
dades_working[dades_working$id == "47917082",            "bathrooms"]      <- NA  # 12 banys
dades_working[dades_working$id == "7394725",             "minimum_nights"] <- NA  # 800 nits
dades_working[dades_working$id == "1047634949162040960", "price"]          <- NA  # 3.930€

cat("\n- Verificació NAs creats:\n")
cat("  price (9999)    :", is.na(dades_working[dades_working$id == "1325166309003730944", "price"]), "\n")
cat("  bathrooms (12)  :", is.na(dades_working[dades_working$id == "47917082",            "bathrooms"]), "\n")
cat("  min_nights (800):", is.na(dades_working[dades_working$id == "7394725",             "minimum_nights"]), "\n")
cat("  price (3930)    :", is.na(dades_working[dades_working$id == "1047634949162040960", "price"]), "\n")

# Dataset numèric base per a les imputacions
cols_numeriques <- c("id", varNum_net)
dades_base      <- dades_working[, cols_numeriques]

# ==============================================================================
# 4A. MICE per als 2 PRICE
# Imputem NOMÉS price, les altres variables les deixem intactes
# ==============================================================================
cat("\n--- IMPUTACIÓ 4A: MICE per a price ---\n")

# Configurem MICE perquè NOMÉS imputi 'price'
metodes_mice <- rep("",  length(cols_numeriques))
names(metodes_mice) <- cols_numeriques
metodes_mice["price"] <- "rf"  # Només imputem price

imputacio_mice <- mice(
  dades_base,
  m      = 1,
  maxit  = 5,
  method = metodes_mice,
  seed   = 123
)
dades_mice <- complete(imputacio_mice)

# Extraiem els dos valors imputats per price
price_mice_9999 <- dades_mice[dades_mice$id == "1325166309003730944", "price"]
price_mice_3930 <- dades_mice[dades_mice$id == "1047634949162040960", "price"]
cat("  price (era 9.999€) -> MICE:", round(price_mice_9999, 2), "\n")
cat("  price (era 3.930€) -> MICE:", round(price_mice_3930, 2), "\n")

# ==============================================================================
# 4B. KNN per a BATHROOMS
# ==============================================================================
cat("\n--- IMPUTACIÓ 4B: KNN per a bathrooms ---\n")

dades_knn <- kNN(dades_base, variable = "bathrooms", k = 5)
# kNN afegeix una columna '_imp' de control, la traiem
dades_knn <- dades_knn[, !grepl("_imp$", names(dades_knn))]

bath_knn <- dades_knn[dades_knn$id == "47917082", "bathrooms"]
cat("  bathrooms (era 12) -> KNN:", round(bath_knn, 2), "\n")

# ==============================================================================
# 4C. MICE vs KNN per a MINIMUM_NIGHTS (comparem els dos)
# ==============================================================================
cat("\n--- IMPUTACIÓ 4C: Comparativa MICE vs KNN per a minimum_nights ---\n")

# MICE per a minimum_nights
metodes_nights <- rep("", length(cols_numeriques))
names(metodes_nights) <- cols_numeriques
metodes_nights["minimum_nights"] <- "rf"

imputacio_nights_mice <- mice(
  dades_base,
  m      = 1,
  maxit  = 5,
  method = metodes_nights,
  seed   = 123
)
nights_mice <- complete(imputacio_nights_mice)[
  complete(imputacio_nights_mice)$id == "7394725", "minimum_nights"
]

# KNN per a minimum_nights
dades_knn_nights <- kNN(dades_base, variable = "minimum_nights", k = 5)
dades_knn_nights <- dades_knn_nights[, !grepl("_imp$", names(dades_knn_nights))]
nights_knn <- dades_knn_nights[dades_knn_nights$id == "7394725", "minimum_nights"]

# Estadístiques de referència per decidir
nights_q75  <- quantile(dades_working$minimum_nights, 0.75, na.rm = TRUE)
nights_med  <- median(dades_working$minimum_nights,         na.rm = TRUE)
nights_mean <- mean(dades_working$minimum_nights,           na.rm = TRUE)

cat("\n  Referència de la distribució de minimum_nights:\n")
cat("    Mediana :", nights_med,  "\n")
cat("    Mitjana :", round(nights_mean, 2), "\n")
cat("    Q75     :", nights_q75, "\n")
cat("\n  Resultat MICE :", nights_mice, "\n")
cat("  Resultat KNN  :", nights_knn,  "\n")
cat("\n  -> El valor més proper a la distribució real serà el millor candidat.\n")

# ==============================================================================
# 5. CONSTRUCCIÓ DEL DATASET FINAL (amb les decisions preses)
# ==============================================================================
# Un cop mireu els resultats de 4C i decidiu quin mètode és millor per
# minimum_nights, canvieu 'nights_escollit' pel valor que preferiu.
# ==============================================================================

nights_escollit <- nights_knn  # <-- CANVIA A nights_knn si és millor

cat("\n--- DECISIÓ FINAL minimum_nights:", nights_escollit, "(MICE escollit per defecte) ---\n")
cat("    Si KNN és millor, canvia 'nights_escollit <- nights_knn' i reexecuta.\n")

# Apliquem totes les imputacions al dataset base
dades_num_imputades <- dades_mice  # Partim del que ja té price imputat per MICE

# Sobreescrivim bathrooms amb KNN
dades_num_imputades[dades_num_imputades$id == "47917082", "bathrooms"] <- bath_knn

# Sobreescrivim minimum_nights amb el mètode escollit
dades_num_imputades[dades_num_imputades$id == "7394725", "minimum_nights"] <- nights_escollit

# ==============================================================================
# AUDITORIA FINAL D'IMPUTACIÓ
# ==============================================================================
comparativa <- data.frame(
  ID               = c("1325166309003730944", "47917082", "7394725", "1047634949162040960"),
  Variable         = c("price", "bathrooms", "minimum_nights", "price"),
  Valor_Original   = c(9999, 12, 800, 3930),
  Metode           = c("MICE", "KNN", "MICE/KNN (decidit)", "MICE"),
  Valor_Imputat    = c(
    round(price_mice_9999, 2),
    round(bath_knn,        2),
    round(nights_escollit, 2),
    round(price_mice_3930, 2)
  )
)
cat("\n--- RESUM DE TOTES LES IMPUTACIONS ---\n")
print(comparativa)

# ==============================================================================
# FASE 5: RECONSTRUCCIÓ DEL DATASET SENCER
# ==============================================================================
columnes_text_netes <- dades_working[, !(names(dades_working) %in% varNum_net)]

dades_totals_netes <- merge(
  columnes_text_netes,
  dades_num_imputades,
  by    = "id",
  all.x = TRUE
)

cat("\n--- AUDITORIA DE FUSIÓ ---\n")
cat("Files finals                              :", nrow(dades_totals_netes), "\n")
cat("Columnes totals                           :", ncol(dades_totals_netes), "\n")
cat("NAs restants                              :", sum(is.na(dades_totals_netes)), "\n")
cat("Pisos del trimming presents (ha de ser 0):", sum(dades_totals_netes$id %in% ids_a_eliminar), "\n")
cat("Room_type Waterwolf                       :", 
    as.character(dades_totals_netes[dades_totals_netes$id == "29874862", "room_type"]), "\n")

# Guardem
saveRDS(dades_totals_netes, "../data/dataset_net_outliers.rds")
write.csv2(dades_totals_netes, "../data/dataset_net_outliers.csv", row.names = FALSE)
cat("\nFitxers RDS i CSV guardats correctament a la carpeta data/.\n")

dadesfinals <- readRDS("../data/dataset_net_outliers.rds")