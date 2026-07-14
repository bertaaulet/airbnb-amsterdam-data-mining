# ===========================================================
# FAMD + CLUSTERING
# ===========================================================
# Objectiu:
# 1) Realitzar una descomposició factorial FAMD.
# 2) Usar les coordenades dels individus per executar un nou clustering.
# 3) Estimar una K inicial visualment amb el biplot FAMD.
# 4) Simular diversos valors de K i seleccionar un K òptim segons
#    Silhouette, Calinski-Harabasz i Dunn.
# 5) Fer el clustering sobre les dimensions FAMD que acumulen aproximadament
#    el 80% de la variància/inèrcia explicada.
# 6) Exportar resultats i gràfics a la carpeta famd_outputs.
# 7) Mostrar igualment tots els gràfics al panell Plots de R/RStudio.
# ===========================================================

library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggplot2)
library(ggrepel)
library(cluster)
library(dplyr)
library(grid)

set.seed(123)

# ===========================================================
# 0. CONFIGURACIÓ GENERAL
# ===========================================================

output_dir <- "famd_outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Funció auxiliar:
# - Desa el ggplot com a PNG.
# - També el mostra al panell Plots de R/RStudio.
save_and_print <- function(plot_obj, filename, width = 10, height = 7, dpi = 300) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
  print(plot_obj)
}

# ===========================================================
# 1. DATASET
# ===========================================================

df <- read.csv("../profiling/dataset_cure_python.csv", stringsAsFactors = FALSE)

# ===========================================================
# 2. SELECCIÓ DE VARIABLES PER AL FAMD
# ===========================================================
# Criteri:
# - Si una variable existeix en versió original i logarítmica, usem NOMÉS la logarítmica.
# - Això evita redundància i que la mateixa informació pesi doble en el FAMD.
# - Les variables suplementàries no construeixen els eixos factorials.
# - El clustering del D3 anterior, si existeix, es deixa com a suplementari.

vars_num_candidates <- c(
  "log_price",
  "log_dies_antiguitat_listing",
  "log_dies_recencia_review",
  "log_estimated_revenue_l365d",
  "log_estimated_occupancy_l365d",
  "log_minimum_nights",
  "log_maximum_nights",
  "log_number_of_reviews",
  "log_reviews_per_month",
  "accommodates",
  "bathrooms",
  "bedrooms",
  "beds",
  "availability_365",
  "dies_antiguitat_host",
  "amenities_score",
  "beds_per_bedroom",
  "distancia_centre_km"
)

vars_num_actives <- intersect(vars_num_candidates, names(df))

vars_cat_actives <- c(
  "room_type",
  "host_is_superhost",
  "host_type",
  "instant_bookable",
  "cat_acceptance_rate",
  "cat_response_rate",
  "cat_review_scores_rating"
)

vars_cat_actives <- intersect(vars_cat_actives, names(df))

# Variables suplementàries categòriques: projectades a posteriori, no construeixen els eixos.
# NO S'INCLOUEN COM ACTIVES pels mateixos motius que al MCA:
# - neighbourhood_cleansed: massa categories (20), distorsiona la inèrcia total
# - property_type: massa categories (11)
# - bath_is_shared: extremadament desbalancejada (94.68% és 0)
# - listing_is_new: variable derivada temporal, redundant amb antiguitat i reviews
# - cluster: variable de resultat/validació del D3, no ha de construir eixos nous

vars_sup_candidates <- c(
  "neighbourhood_cleansed",
  "property_type",
  "bath_is_shared",
  "listing_is_new",
  "cluster"
)

vars_sup <- intersect(vars_sup_candidates, names(df))

if (length(vars_num_actives) == 0) {
  stop("No s'ha trobat cap variable quantitativa candidata. Revisa els noms de columnes.")
}

if (length(vars_cat_actives) == 0) {
  stop("No s'ha trobat cap variable categòrica activa. Revisa els noms de columnes.")
}

vars_famd <- c(vars_num_actives, vars_cat_actives, vars_sup)
df.famd   <- df[, vars_famd]

# ===========================================================
# 3. NETEJA I TIPAT DE VARIABLES
# ===========================================================

# --- 3A. Variables numèriques ---

clean_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("\\$", "", x)
  x <- gsub(",",   "", x)
  x <- gsub("%",   "", x)
  as.numeric(x)
}

for (v in vars_num_actives) {
  df.famd[[v]] <- clean_numeric(df.famd[[v]])
}

# Imputació de valors perduts numèrics amb la mediana
for (v in vars_num_actives) {
  med <- median(df.famd[[v]], na.rm = TRUE)
  df.famd[[v]][is.na(df.famd[[v]])] <- med
}

# Eliminació de variables numèriques amb variància zero
vars_num_zero_var <- vars_num_actives[
  sapply(df.famd[, vars_num_actives, drop = FALSE], function(x) var(x, na.rm = TRUE) == 0)
]

if (length(vars_num_zero_var) > 0) {
  message("Variables numèriques eliminades per variància zero: ",
          paste(vars_num_zero_var, collapse = ", "))
  df.famd        <- df.famd[, !(names(df.famd) %in% vars_num_zero_var)]
  vars_num_actives <- setdiff(vars_num_actives, vars_num_zero_var)
}

# --- 3B. Variables categòriques: idèntic al MCA ---

vars_cat_all <- c(vars_cat_actives, vars_sup)

# Conversió a factor i imputació de buits/NA amb "Missing"
for (v in vars_cat_all) {
  x <- as.character(df.famd[[v]])
  x[is.na(x) | x == ""] <- "Missing"
  df.famd[[v]] <- as.factor(x)
}

# Reetiquetatge de categories actives (format nomvariable_tag, igual que al MCA)
# Nota: els levels() han de coincidir amb l'ordre alfabètic actual del factor;
#       si el dataset canvia, revisa amb levels(df.famd$<var>) abans d'executar.

levels(df.famd$room_type) <- paste0("room_",
                                    c("EntireHome", "HotelRoom", "PrivateRoom", "SharedRoom"))

levels(df.famd$host_is_superhost) <- paste0("superhost_",
                                            c("No", "Yes"))

levels(df.famd$host_type) <- paste0("hosttype_",
                                    c("Particular", "Professional"))

levels(df.famd$instant_bookable) <- paste0("instant_",
                                           c("No", "Yes"))

levels(df.famd$cat_acceptance_rate) <- paste0("accept_",
                                              c("MoltRestrictiu", "Selectiu", "Total"))

levels(df.famd$cat_response_rate) <- paste0("response_",
                                            c("Alta", "Baixa", "Immediata", "Moderada"))

levels(df.famd$cat_review_scores_rating) <- paste0("review_",
                                                   c("Excellent", "Estandard", "Millorable", "Nou"))

# Eliminació de variables categòriques amb un sol nivell
vars_cat_one_level <- vars_cat_all[
  sapply(df.famd[, vars_cat_all, drop = FALSE], function(x) nlevels(x) <= 1)
]

if (length(vars_cat_one_level) > 0) {
  message("Variables categòriques eliminades per tenir una sola categoria: ",
          paste(vars_cat_one_level, collapse = ", "))
  df.famd          <- df.famd[, !(names(df.famd) %in% vars_cat_one_level)]
  vars_cat_actives <- setdiff(vars_cat_actives, vars_cat_one_level)
  vars_sup         <- setdiff(vars_sup, vars_cat_one_level)
}

# --- 3C. Construcció del dataframe final i índex suplementaris ---

vars_active_final <- c(vars_num_actives, vars_cat_actives)
vars_sup_final    <- vars_sup

df.famd <- df.famd[, c(vars_active_final, vars_sup_final)]

idx_sup <- if (length(vars_sup_final) > 0) {
  (length(vars_active_final) + 1):ncol(df.famd)
} else {
  NULL
}

# ===========================================================
# 4. EXPLORACIÓ PRÈVIA (només consola, sense gràfics)
# ===========================================================

cat("Variables quantitatives actives:\n")
print(vars_num_actives)

cat("Variables qualitatives actives:\n")
print(vars_cat_actives)

cat("Variables suplementàries:\n")
print(vars_sup_final)

str(df.famd)
summary(df.famd)

# ===========================================================
# 5. FAMD — DESCOMPOSICIÓ FACTORIAL
# ===========================================================

ncp_famd <- min(25, ncol(df.famd) - 1, nrow(df.famd) - 1)

res.famd <- FAMD(
  df.famd,
  ncp = ncp_famd,
  sup.var = idx_sup,
  graph = FALSE
)

print(res.famd$eig)

# ===========================================================
# 6. EIGENVALUES I RETENCIÓ DE DIMENSIONS
# ===========================================================

eig.val <- as.data.frame(get_eigenvalue(res.famd))

print(eig.val)

p_scree <- fviz_screeplot(res.famd, addlabels = TRUE, ylim = c(0, 50)) +
  labs(
    title = "FAMD — Scree plot",
    subtitle = "Percentatge d'inèrcia explicada per dimensió"
  )

p_scree <- fviz_screeplot(
  res.famd,
  addlabels = TRUE,
  ylim = c(0, 50),
  barfill = "#cc3333",
  barcolor = "#cc3333",
  labelsize = 5.2
) +
  labs(
    title = "FAMD: Scree plot",
    subtitle = "Percentatge d'inèrcia explicada per dimensió",
    x = "Dimensions",
    y = "Percentatge d'inèrcia"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "grey35"),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.line = element_line(color = "grey70"),
    legend.position = "none"
  )

save_and_print(
  p_scree,
  filename = "famd_screeplot.png",
  width = 9,
  height = 6
)

# Dimensions per interpretar a l'informe:
# Malgrat que el criteri del colze apunta a 4 dimensions, es mantenen 3
# per coherència amb l'anàlisi MCA i per facilitar la interpretació visual
# (3 biplots en lloc de 6). El 37.44% d'inèrcia acumulada és acceptable
# en un FAMD amb 25 variables mixtes, on la inèrcia es distribueix
# naturalment entre més dimensions.
n_dim_report <- 3

cat("Inèrcia acumulada en les 3 primeres dimensions:\n")
print(eig.val[1:n_dim_report, ])

# Dimensions per al clustering:
# Segons el criteri actual del projecte, el clustering s'ha de fer sobre
# les dimensions que expliquen aproximadament fins al 80% de la variància/inèrcia.
cum_col <- grep("cumulative", colnames(eig.val), ignore.case = TRUE, value = TRUE)[1]

if (is.na(cum_col)) {
  stop("No s'ha trobat la columna d'inèrcia acumulada a eig.val.")
}

n_dim_cluster <- which(eig.val[[cum_col]] >= 80)[1]

if (is.na(n_dim_cluster)) {
  n_dim_cluster <- nrow(eig.val)
}

#Segons les  sortides, el 80% s'assoleix a Dim 15.
n_dim_cluster <- 15

cat("Nombre de dimensions utilitzades per al clustering:", n_dim_cluster, "\n")
cat("Inèrcia acumulada amb les dimensions del clustering:\n")
print(eig.val[1:n_dim_cluster, ])

# ===========================================================
# 7. ANÀLISI DE DIMENSIONS FAMD
# ===========================================================

var <- get_famd_var(res.famd)
ind <- get_famd_ind(res.famd)

# ===========================================================
# 7.1 VARIABLES (numèriques i categòriques juntes)
# ===========================================================
# Gràfics equivalents als de l'MCA: pseudo-correlació i coordenades de categories,
# acolorits per contribució. Només s'etiqueten les 5 variables/categories
# que més contribueixen a cada parell de dimensions.

plot_famd_var_top5 <- function(res_famd, dim_x, dim_y) {
  var_obj   <- get_famd_var(res_famd)
  score     <- var_obj$contrib[, dim_x] + var_obj$contrib[, dim_y]
  top10_names <- names(sort(score, decreasing = TRUE))[1:10]
  
  fviz_famd_var(
    res_famd,
    axes      = c(dim_x, dim_y),
    repel     = TRUE,
    col.var   = "contrib",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    select.var = list(name = top10_names),
    ggtheme   = theme_minimal()
  ) +
    labs(
      title    = paste0("FAMD — Variables Dim ", dim_x, " vs Dim ", dim_y),
      subtitle = "Totes les variables; etiquetes de les 10 amb major contribució"
    )
}

# Per mostrar TOTES les variables acolorides però etiquetar només les top 5,
# combinem un fviz sense etiquetes + geom_text_repel manual de les top 5.
plot_famd_var_contrib_top5 <- function(res_famd, dim_x, dim_y) {
  var_obj   <- get_famd_var(res_famd)
  score     <- var_obj$contrib[, dim_x] + var_obj$contrib[, dim_y]
  top10_names <- names(sort(score, decreasing = TRUE))[1:10]
  
  contrib_sum <- rowSums(var_obj$contrib[, c(dim_x, dim_y)])
  
  df_vars <- data.frame(
    x       = var_obj$coord[, dim_x],
    y       = var_obj$coord[, dim_y],
    contrib = contrib_sum,
    label   = rownames(var_obj$coord)
  )
  df_top10 <- df_vars[df_vars$label %in% top10_names, ]
  
  ggplot(df_vars, aes(x = x, y = y, color = contrib)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    geom_point(size = 2.5) +
    scale_color_gradient2(
      low = "#00AFBB", mid = "#E7B800", high = "#FC4E07",
      midpoint = median(contrib_sum)
    ) +
    geom_text_repel(
      data        = df_top10,
      aes(label   = label),
      fontface    = "bold",
      size        = 3.5,
      max.overlaps = Inf,
      color       = "black"
    ) +
    theme_minimal() +
    labs(
      title    = paste0("FAMD — Variables Dim ", dim_x, " vs Dim ", dim_y),
      subtitle = "Totes les variables acolorides per contribució; etiquetes top 10",
      x        = paste0("Dim ", dim_x),
      y        = paste0("Dim ", dim_y),
      color    = "Contribució"
    )
}

p_var_12 <- plot_famd_var_contrib_top5(res.famd, 1, 2)
save_and_print(p_var_12, "famd_variables_dim1_dim2.png", width = 9, height = 7)

p_var_13 <- plot_famd_var_contrib_top5(res.famd, 1, 3)
save_and_print(p_var_13, "famd_variables_dim1_dim3.png", width = 9, height = 7)

p_var_23 <- plot_famd_var_contrib_top5(res.famd, 2, 3)
save_and_print(p_var_23, "famd_variables_dim2_dim3.png", width = 9, height = 7)

# Gràfics de contribució per dimensió (equivalent a fviz_contrib del MCA)
p_contrib_dim1 <- fviz_contrib(res.famd, choice = "var", axes = 1, top = 15) +
  labs(title = "FAMD — Contribució de variables a Dim 1")
save_and_print(p_contrib_dim1, "famd_contrib_dim1.png", width = 10, height = 6)

p_contrib_dim2 <- fviz_contrib(res.famd, choice = "var", axes = 2, top = 15) +
  labs(title = "FAMD — Contribució de variables a Dim 2")
save_and_print(p_contrib_dim2, "famd_contrib_dim2.png", width = 10, height = 6)

p_contrib_dim3 <- fviz_contrib(res.famd, choice = "var", axes = 3, top = 15) +
  labs(title = "FAMD — Contribució de variables a Dim 3")
save_and_print(p_contrib_dim3, "famd_contrib_dim3.png", width = 10, height = 6)

# ===========================================================
# 7.2 INDIVIDUS
# ===========================================================
# Tots els individus en gris clar. Els 100 que més contribueixen a cada
# parell de dimensions es ressalten acolorits per cos².

plot_famd_ind_all_top100 <- function(res_famd, ind_obj, dim_x, dim_y, n_top = 100) {
  score    <- ind_obj$contrib[, dim_x] + ind_obj$contrib[, dim_y]
  top_idx  <- order(score, decreasing = TRUE)[1:min(n_top, length(score))]
  
  df_all <- data.frame(
    x     = ind_obj$coord[, dim_x],
    y     = ind_obj$coord[, dim_y],
    is_top = FALSE
  )
  df_all$is_top[top_idx] <- TRUE
  
  df_top <- df_all[df_all$is_top, ]
  df_top$cos2 <- ind_obj$cos2[top_idx, dim_x] + ind_obj$cos2[top_idx, dim_y]
  
  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    # Tots els individus en gris de fons
    geom_point(
      data  = df_all[!df_all$is_top, ],
      aes(x = x, y = y),
      color = "grey80", alpha = 0.30, size = 0.6
    ) +
    # Top 100 ressaltats amb gradient de cos²
    geom_point(
      data = df_top,
      aes(x = x, y = y, color = cos2),
      alpha = 0.85, size = 1.8
    ) +
    scale_color_gradient2(
      low      = "#00AFBB",
      mid      = "#E7B800",
      high     = "#FC4E07",
      midpoint = median(df_top$cos2)
    ) +
    theme_minimal() +
    labs(
      title    = paste0("FAMD — Individus Dim ", dim_x, " vs Dim ", dim_y),
      subtitle = paste0("Tots els individus (gris) | Top ", n_top,
                        " per contribució ressaltats (color = cos²)"),
      x        = paste0("Dim ", dim_x),
      y        = paste0("Dim ", dim_y),
      color    = "cos²"
    )
}

p_ind_12 <- plot_famd_ind_all_top100(res.famd, ind, 1, 2)
save_and_print(p_ind_12, "famd_individus_dim1_dim2.png", width = 9, height = 7)

p_ind_13 <- plot_famd_ind_all_top100(res.famd, ind, 1, 3)
save_and_print(p_ind_13, "famd_individus_dim1_dim3.png", width = 9, height = 7)

p_ind_23 <- plot_famd_ind_all_top100(res.famd, ind, 2, 3)
save_and_print(p_ind_23, "famd_individus_dim2_dim3.png", width = 9, height = 7)

# ===========================================================
# 7.3 BIPLOTS MIXTOS
# ===========================================================
# Tots els individus en gris + fletxes de les top n_var variables numèriques
# que més contribueixen a cada parell de dimensions.

plot_famd_biplot_top <- function(res_famd, ind_obj, dim_x, dim_y, n_var = 10) {
  
  # --- Tots els individus en gris ---
  df_ind <- data.frame(
    x = ind_obj$coord[, dim_x],
    y = ind_obj$coord[, dim_y]
  )
  
  # --- Top n_var variables numèriques per contribució ---
  quanti_obj   <- get_famd_var(res_famd, "quanti.var")
  quanti_score <- quanti_obj$contrib[, dim_x] + quanti_obj$contrib[, dim_y]
  top_idx      <- order(quanti_score, decreasing = TRUE)[1:min(n_var, length(quanti_score))]
  
  df_quanti <- data.frame(
    x     = quanti_obj$coord[top_idx, dim_x],
    y     = quanti_obj$coord[top_idx, dim_y],
    label = rownames(quanti_obj$coord)[top_idx]
  )
  
  # --- Escala fletxes a l'espai dels individus ---
  scale_factor <- min(
    max(abs(df_ind$x)) / max(abs(df_quanti$x)),
    max(abs(df_ind$y)) / max(abs(df_quanti$y))
  ) * 0.8
  
  df_quanti$x_sc <- df_quanti$x * scale_factor
  df_quanti$y_sc <- df_quanti$y * scale_factor
  
  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    geom_point(
      data  = df_ind,
      aes(x = x, y = y),
      color = "grey75", alpha = 0.25, size = 0.6
    ) +
    geom_segment(
      data = df_quanti,
      aes(x = 0, y = 0, xend = x_sc, yend = y_sc),
      arrow     = arrow(length = unit(0.20, "cm")),
      linewidth = 0.6,
      color     = "#FC4E07"
    ) +
    geom_text_repel(
      data         = df_quanti,
      aes(x = x_sc, y = y_sc, label = label),
      size         = 3.2,
      fontface     = "bold",
      color        = "#FC4E07",
      max.overlaps = Inf
    ) +
    theme_minimal() +
    labs(
      title    = paste0("FAMD — Biplot Dim ", dim_x, " vs Dim ", dim_y),
      subtitle = paste0("Tots els individus (gris) | Top ", n_var,
                        " variables numèriques per contribució (fletxes)"),
      x = paste0("Dim ", dim_x),
      y = paste0("Dim ", dim_y)
    )
}

p_biplot_12 <- plot_famd_biplot_top(res.famd, ind, 1, 2, n_var = 10)
save_and_print(p_biplot_12, "famd_biplot_dim1_dim2.png", width = 10, height = 8)

p_biplot_13 <- plot_famd_biplot_top(res.famd, ind, 1, 3, n_var = 10)
save_and_print(p_biplot_13, "famd_biplot_dim1_dim3.png", width = 10, height = 8)

p_biplot_23 <- plot_famd_biplot_top(res.famd, ind, 2, 3, n_var = 10)
save_and_print(p_biplot_23, "famd_biplot_dim2_dim3.png", width = 10, height = 8)

# ===========================================================
# 8. CLUSTERING K-MEANS SOBRE COORDENADES FAMD
# ===========================================================
# Input: coordenades dels individus en les n_dim_cluster dimensions FAMD
# (les que acumulen ~80% d'inèrcia). Totes les variables originals queden
# codificades numèricament en aquestes dimensions.

coords_cluster <- as.data.frame(ind$coord[, 1:n_dim_cluster])
dist_famd      <- dist(coords_cluster)

# --- Funcions d'índexs ---

calinski_harabasz <- function(data, cluster) {
  data <- as.matrix(data)
  n    <- nrow(data)
  k    <- length(unique(cluster))
  overall_center <- colMeans(data)
  WSS <- 0; BSS <- 0
  for (cl in unique(cluster)) {
    data_cl   <- data[cluster == cl, , drop = FALSE]
    center_cl <- colMeans(data_cl)
    WSS <- WSS + sum(rowSums((data_cl - matrix(center_cl, nrow(data_cl), ncol(data), byrow = TRUE))^2))
    BSS <- BSS + nrow(data_cl) * sum((center_cl - overall_center)^2)
  }
  (BSS / (k - 1)) / (WSS / (n - k))
}

dunn_index <- function(data, cluster) {
  data     <- as.matrix(data)
  clusters <- unique(cluster)
  d        <- as.matrix(dist(data))
  intra_max <- 0
  for (cl in clusters) {
    idx <- which(cluster == cl)
    if (length(idx) > 1) intra_max <- max(intra_max, max(d[idx, idx]))
  }
  if (intra_max == 0) return(NA)
  inter_min <- Inf
  for (i in 1:(length(clusters) - 1)) {
    for (j in (i + 1):length(clusters)) {
      idx_i <- which(cluster == clusters[i])
      idx_j <- which(cluster == clusters[j])
      inter_min <- min(inter_min, min(d[idx_i, idx_j]))
    }
  }
  inter_min / intra_max
}

# ===========================================================
# 8.1 SIMULACIÓ DE K (punt de partida visual: k = 3)
# ===========================================================
# K inicial obtinguda del biplot FAMD Dim 1 vs Dim 2.
# Es simulen K de 2 a 7 per determinar el K òptim.

k_start_visual <- 3
k_values       <- 2:7

metrics_k <- data.frame(
  K                 = k_values,
  WSS               = NA,
  Silhouette        = NA,
  Calinski_Harabasz = NA,
  Dunn              = NA
)

km_models <- list()

for (k in k_values) {
  km <- kmeans(coords_cluster, centers = k, nstart = 50,
               iter.max = 100, algorithm = "Lloyd")
  km_models[[as.character(k)]] <- km
  sil <- silhouette(km$cluster, dist_famd)
  metrics_k[metrics_k$K == k, "WSS"]               <- sum(km$withinss)
  metrics_k[metrics_k$K == k, "Silhouette"]        <- mean(sil[, 3])
  metrics_k[metrics_k$K == k, "Calinski_Harabasz"] <- calinski_harabasz(coords_cluster, km$cluster)
  metrics_k[metrics_k$K == k, "Dunn"]              <- dunn_index(coords_cluster, km$cluster)
}

print(metrics_k)

# --- Gràfics dels 4 índexs ---

p_wss <- ggplot(metrics_k, aes(x = K, y = WSS)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(title = "K-means — Criteri del colze (WSS)",
       subtitle = paste0("Millor K: inflexió de la corba (colze) | Dimensions FAMD: ", n_dim_cluster),
       x = "Nombre de clusters K", y = "Within-cluster SS")
save_and_print(p_wss, "clustering_wss_colze.png", width = 8, height = 5)

p_sil <- ggplot(metrics_k, aes(x = K, y = Silhouette)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(title = "K-means — Silhouette mitjana",
       subtitle = paste0("Millor K: valor màxim | Dimensions FAMD: ", n_dim_cluster),
       x = "Nombre de clusters K", y = "Silhouette mitjana")
save_and_print(p_sil, "clustering_silhouette.png", width = 8, height = 5)

p_ch <- ggplot(metrics_k, aes(x = K, y = Calinski_Harabasz)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(title = "K-means — Calinski-Harabasz",
       subtitle = paste0("Millor K: valor màxim | Dimensions FAMD: ", n_dim_cluster),
       x = "Nombre de clusters K", y = "CH index")
save_and_print(p_ch, "clustering_calinski_harabasz.png", width = 8, height = 5)

p_dunn <- ggplot(metrics_k, aes(x = K, y = Dunn)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(title = "K-means — Dunn index",
       subtitle = paste0("Millor K: valor màxim | Dimensions FAMD: ", n_dim_cluster),
       x = "Nombre de clusters K", y = "Dunn index")
save_and_print(p_dunn, "clustering_dunn.png", width = 8, height = 5)

# --- Ranking agregat per seleccionar K òptim ---

metrics_k$rank_sil   <- rank(-metrics_k$Silhouette,        ties.method = "min")
metrics_k$rank_ch    <- rank(-metrics_k$Calinski_Harabasz, ties.method = "min")
metrics_k$rank_dunn  <- rank(-metrics_k$Dunn,              ties.method = "min")
metrics_k$rank_total <- metrics_k$rank_sil + metrics_k$rank_ch + metrics_k$rank_dunn

cat("Ranking de K segons Silhouette, CH i Dunn:\n")
print(metrics_k[order(metrics_k$rank_total), ])

k_opt <- metrics_k$K[which.min(metrics_k$rank_total)]
cat("K òptim seleccionat:", k_opt, "\n")

# ===========================================================
# 8.2 CLUSTERING FINAL I CARACTERITZACIÓ
# ===========================================================
# Un cop decidit k_opt, es genera el clustering final i es visualitza
# sobre el pla principal Dim 1 vs Dim 2 del FAMD.

km_final <- km_models[[as.character(k_opt)]]

df$cluster_famd      <- as.factor(km_final$cluster)
df.famd$cluster_famd <- as.factor(km_final$cluster)

# --- Visualització sobre el pla factorial ---

p_cluster_final <- fviz_famd_ind(
  res.famd,
  axes        = c(1, 2),
  geom        = "point",
  label       = "none",
  habillage   = df$cluster_famd,
  addEllipses = TRUE,
  ellipse.type = "confidence",
  alpha.ind   = 0.35,
  ggtheme     = theme_minimal()
) +
  labs(
    title    = paste0("FAMD — Clustering final (K = ", k_opt, ") sobre Dim 1 vs Dim 2"),
    subtitle = paste0("K visual inicial = ", k_start_visual,
                      " | K òptim = ", k_opt,
                      " | Dimensions usades = ", n_dim_cluster)
  )
save_and_print(p_cluster_final,
               paste0("famd_clusters_final_k", k_opt, ".png"),
               width = 9, height = 7)

# Validació creuada amb el cluster del D3 (si existeix)
if ("cluster" %in% names(df)) {
  cat("Taula de contingència Cluster D3 vs Cluster FAMD:\n")
  print(table(Cluster_D3 = df$cluster, Cluster_FAMD = df$cluster_famd))
}

# ===========================================================
# REETIQUETATGE DEL CLUSTER FAMD PER MÀXIMA COINCIDÈNCIA AMB D3
# ===========================================================
# K-means assigna etiquetes arbitràries. Reemparellam cada cluster FAMD
# amb el cluster D3 amb el qual té màxima coincidència (assignació greedy).

contingency <- table(Cluster_D3 = df$cluster, Cluster_FAMD = df$cluster_famd)
print(contingency)

# Per cada cluster FAMD, troba el cluster D3 amb més observacions en comú
famd_levels <- colnames(contingency)
d3_levels   <- rownames(contingency)

mapping <- integer(length(famd_levels))
names(mapping) <- famd_levels

assigned_d3 <- c()
for (fk in famd_levels) {
  col_counts <- contingency[, fk]
  col_counts[assigned_d3] <- 0  # no reutilitzar D3 ja assignat
  best_d3 <- names(which.max(col_counts))
  mapping[fk] <- best_d3
  assigned_d3 <- c(assigned_d3, best_d3)
}

cat("Mapping FAMD → D3:\n")
print(mapping)

# Aplica el reetiquetatge
df$cluster_famd_relab      <- as.factor(mapping[as.character(df$cluster_famd)])
df.famd$cluster_famd_relab <- df$cluster_famd_relab

cat("\nTaula de contingència DESPRÉS del reetiquetatge:\n")
print(table(Cluster_D3 = df$cluster, Cluster_FAMD = df$cluster_famd_relab))

# ===========================================================
# GRÀFIC: INDIVIDUS DISCORDANTS ENTRE D3 I FAMD
# ===========================================================
# Classifiquem cada individu com "Concordant" o "Discordant" i el
# pintem sobre el pla Dim 1 vs Dim 2 del FAMD.

df$concordancia <- ifelse(
  as.character(df$cluster) == as.character(df$cluster_famd_relab),
  "Concordant",
  "Discordant"
)

ind_coord_plot <- as.data.frame(ind$coord[, 1:2])
colnames(ind_coord_plot) <- c("Dim1", "Dim2")
ind_coord_plot$concordancia  <- df$concordancia
ind_coord_plot$cluster_d3    <- as.factor(df$cluster)

# Separem per capes: concordants al fons, discordants al davant
df_conc <- ind_coord_plot[ind_coord_plot$concordancia == "Concordant", ]
df_disc <- ind_coord_plot[ind_coord_plot$concordancia == "Discordant",  ]

cat(sprintf("\nIndividus concordants: %d (%.1f%%)\n",
            nrow(df_conc), 100 * nrow(df_conc) / nrow(ind_coord_plot)))
cat(sprintf("Individus discordants: %d (%.1f%%)\n",
            nrow(df_disc), 100 * nrow(df_disc) / nrow(ind_coord_plot)))

p_discordants <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  # Concordants: punts de fons acolorits per cluster D3
  geom_point(
    data  = df_conc,
    aes(x = Dim1, y = Dim2, color = cluster_d3),
    alpha = 0.20, size = 0.6
  ) +
  # Discordants: mateix color però amb contorn negre i lleument més grans
  geom_point(
    data   = df_disc,
    aes(x = Dim1, y = Dim2, color = cluster_d3),
    size   = 1.8, alpha = 0.85,
    shape  = 21,           # cercle amb contorn independent del farcit
    fill   = NA,           # farcit transparent → es veu el color de sota
    stroke = 0.6           # gruix del contorn
  ) +
  scale_color_manual(
    values = c("1" = "#00AFBB", "2" = "#E7B800", "3" = "#FC4E07"),
    name   = "Cluster D3"
  ) +
  theme_minimal() +
  labs(
    title    = "Concordança entre Cluster D3 i Cluster FAMD",
    subtitle = sprintf(
      "Cercles amb contorn = discordants (%d individus, %.1f%%) | Colors = cluster D3 original",
      nrow(df_disc), 100 * nrow(df_disc) / nrow(ind_coord_plot)
    ),
    x = "Dim 1", y = "Dim 2"
  )


save_and_print(p_discordants, "famd_concordancia_clusters.png", width = 10, height = 7)

# --- Caracterització dels clusters ---

# Mides
cluster_sizes <- df %>%
  group_by(cluster_famd) %>%
  summarise(n = n(), pct = round(100 * n() / nrow(df), 2), .groups = "drop")
print(cluster_sizes)

# Resum numèric per cluster (mitjana i mediana)
numeric_summary <- df.famd %>%
  mutate(cluster_famd = df$cluster_famd) %>%
  group_by(cluster_famd) %>%
  summarise(
    across(all_of(vars_num_actives),
           list(mean = ~mean(.x, na.rm = TRUE),
                median = ~median(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )
print(numeric_summary)

# Categoria dominant per variable i cluster
dominant_category <- function(data, cluster_var, cat_var) {
  data %>%
    group_by(.data[[cluster_var]], .data[[cat_var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(.data[[cluster_var]]) %>%
    mutate(pct = round(100 * n / sum(n), 2)) %>%
    arrange(.data[[cluster_var]], desc(n)) %>%
    slice(1) %>%
    ungroup() %>%
    rename(cluster = 1, category = 2) %>%
    mutate(variable = cat_var) %>%
    select(cluster, variable, category, n, pct)
}

cat_dominants <- bind_rows(
  lapply(vars_cat_actives, function(v) {
    dominant_category(
      df.famd %>% mutate(cluster_famd = df$cluster_famd),
      "cluster_famd", v
    )
  })
)
print(cat_dominants)

# Distribució completa de categories per cluster
for (v in vars_cat_actives) {
  cat(sprintf("\nDistribució de %s per cluster:\n", v))
  print(round(prop.table(table(df$cluster_famd, df.famd[[v]]), margin = 1) * 100, 2))
}

