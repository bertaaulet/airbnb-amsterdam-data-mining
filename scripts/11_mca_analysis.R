# ===========================================================
# MCA
# ===========================================================

library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(dplyr)

df <- read.csv("../data/dataset_cure_python.csv", stringsAsFactors = FALSE)

vars_actives <- c("room_type", "host_is_superhost",
                  "host_type", "instant_bookable",
                  "cat_acceptance_rate", "cat_response_rate",
                  "cat_review_scores_rating")

# Variables suplementàries: no construeixen els eixos,
# es projecten a posteriori com a informació complementària
vars_sup <- c("neighbourhood_cleansed", "property_type",
              "bath_is_shared", "listing_is_new")

# NO S'INCLOUEN COM ACTIVES:
# neighbourhood_cleansed: perquè té moltes categories (20) i afecta massa a la inèrcia total
# property_type: té masses categories també (11)
# bath_is_shared: extremadament desbalancejat: 94.68% és 0
# listing_is_new: no descriu els apartaments en si, sinó que és una conseqüència de quan es recullen les dades

df.active <- df[, c(vars_actives, vars_sup)]
df.active <- data.frame(lapply(df.active, as.factor))

# ===========================================================
# REETIQUETATGE: format nomvariable_tag (només actives)
# ===========================================================

levels(df.active$room_type) <- paste0("room_",
                                      c("EntireHome", "HotelRoom", "PrivateRoom", "SharedRoom"))

levels(df.active$host_is_superhost) <- paste0("superhost_",
                                              c("No", "Yes"))

levels(df.active$host_type) <- paste0("hosttype_",
                                      c("Particular", "Professional"))

levels(df.active$instant_bookable) <- paste0("instant_",
                                             c("No", "Yes"))

levels(df.active$cat_acceptance_rate) <- paste0("accept_",
                                                c("MoltRestrictiu", "Selectiu", "Total"))

levels(df.active$cat_response_rate) <- paste0("response_",
                                              c("Alta", "Baixa", "Immediata", "Moderada"))

levels(df.active$cat_review_scores_rating) <- paste0("review_",
                                                     c("Excellent", "Estandard", "Millorable", "Nou"))

# ===========================================================
# EXPLORACIÓ DE DISTRIBUCIÓ DE CATEGORIES (només actives)
# ===========================================================

for (i in 1:length(vars_actives)) {
  plot(df.active[, i],
       main = colnames(df.active)[i],
       ylab = "Count",
       col  = "steelblue",
       las  = 2)
}

# ===========================================================
# MCA — amb variables suplementàries
# ===========================================================

# Índex de les columnes suplementàries dins de df.active
idx_sup <- (length(vars_actives) + 1):ncol(df.active)

res.mca <- MCA(df.active,
               quali.sup = idx_sup,
               method    = "Indicator",
               graph     = FALSE)

print(res.mca)

# ===========================================================
# 1. – Eigenvalues i Variance
# ===========================================================

eig.val <- get_eigenvalue(res.mca)
eig.val

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45), barfill = "#4E79A7", barcolor = "#4E79A7")

# Veiem, aplicant la regla del Colze, que ens quedem amb les 3 primeres dimensions, que expliquen el 40.5% de la variància total (inèrcia).

# ==============================================================================
# 2. ANÀLISI DE VARIABLES (Apartat 2.2 Informe)
# ==============================================================================
var <- get_mca_var(res.mca)

# A) Pseudo-correlació global (Identificació general d'eixos)
fviz_mca_var(res.mca, choice = "mca.cor", axes = c(1, 2), invisible = "quali.sup", 
             repel = TRUE, max.overlaps = Inf, ggtheme = theme_minimal()) +
  labs(title = "Pseudo-correlació de variables actives – Dim 1 vs 2")

fviz_mca_var(res.mca, choice = "mca.cor", axes = c(1, 3), invisible = "quali.sup", 
             repel = TRUE, max.overlaps = Inf, ggtheme = theme_minimal()) +
  labs(title = "Pseudo-correlació de variables actives – Dim 1 vs 3")

fviz_mca_var(res.mca, choice = "mca.cor", axes = c(2, 3), invisible = "quali.sup", 
             repel = TRUE, max.overlaps = Inf, ggtheme = theme_minimal()) +
  labs(title = "Pseudo-correlació de variables actives – Dim 2 vs 3")

# B) Coordenades exactes de les categories (Oposicions d'extrems)
fviz_mca_var(res.mca, axes = c(1, 2), invisible = "quali.sup", repel = TRUE, 
             max.overlaps = Inf, force = 2, ggtheme = theme_minimal()) +
  labs(title = "Coordenades de categories actives – Dim 1 vs 2")

fviz_mca_var(res.mca, axes = c(1, 3), invisible = "quali.sup", repel = TRUE, 
             max.overlaps = Inf, force = 2, ggtheme = theme_minimal()) +
  labs(title = "Coordenades de categories actives – Dim 1 vs 3")

fviz_mca_var(res.mca, axes = c(2, 3), invisible = "quali.sup", repel = TRUE, 
             max.overlaps = Inf, force = 2, ggtheme = theme_minimal()) +
  labs(title = "Coordenades de categories actives – Dim 2 vs 3")


# ==============================================================================
# 3. ANÀLISI D'INDIVIDUS (Apartat 2.2 Informe)
# ==============================================================================
ind_res <- get_mca_ind(res.mca)

# Contribució Top 50 Global (Mostra la inèrcia micro-fragmentada <0.125%)
fviz_contrib(res.mca, choice = "ind", axes = 1:3, top = 50) +
  labs(title = "Top 50 individus més contribuents – Dim 1+2+3")

# Funció helper per crear gràfics de punts acolorits amb Top 10 etiquetes
plot_ind_top10 <- function(dim_x, dim_y, res_mca, ind_obj) {
  top10_idx <- order(ind_obj$contrib[,dim_x] + ind_obj$contrib[,dim_y], decreasing = TRUE)[1:10]
  top10_df <- data.frame(
    x = ind_obj$coord[top10_idx, dim_x],
    y = ind_obj$coord[top10_idx, dim_y],
    label = rownames(ind_obj$coord)[top10_idx]
  )
  fviz_mca_ind(res_mca, axes = c(dim_x, dim_y), geom = "point", col.ind = "contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), alpha.ind = 0.5, 
               ggtheme = theme_minimal()) +
    geom_text_repel(data = top10_df, aes(x = x, y = y, label = label), 
                    color = "black", fontface = "bold", max.overlaps = Inf) +
    labs(title = paste0("Individus per contribució – Dim ", dim_x, " vs ", dim_y),
         subtitle = "Etiquetes dels 10 individus amb major aportació")
}

print(plot_ind_top10(1, 2, res.mca, ind_res))
print(plot_ind_top10(1, 3, res.mca, ind_res))
print(plot_ind_top10(2, 3, res.mca, ind_res))


# ==============================================================================
# 4. BIPLOTS FINALS (Apartat 2.3 Informe)
# ==============================================================================
plot_biplot_top15 <- function(dim_x, dim_y, res_mca, ind_obj) {
  top15_idx <- order(ind_obj$contrib[,dim_x] + ind_obj$contrib[,dim_y], decreasing = TRUE)[1:15]
  df_ind <- data.frame(
    x = ind_obj$coord[top15_idx, dim_x],
    y = ind_obj$coord[top15_idx, dim_y],
    label = rownames(ind_obj$coord)[top15_idx]
  )
  fviz_mca_var(res_mca, axes = c(dim_x, dim_y), invisible = "quali.sup",
               col.var = "#FC4E07", shape.var = 17, repel = TRUE, 
               max.overlaps = Inf, ggtheme = theme_minimal()) +
    geom_point(data = df_ind, aes(x = x, y = y), color = "#00AFBB", size = 2) +
    geom_text_repel(data = df_ind, aes(x = x, y = y, label = label), 
                    color = "#00AFBB", fontface = "bold", box.padding = 0.5) +
    labs(title = paste0("Biplot Principal – Dim ", dim_x, " vs ", dim_y),
         subtitle = "Categories Actives (Vermell) vs Top 15 Individus Contribuents (Blau)")
}

print(plot_biplot_top15(1, 2, res.mca, ind_res))
print(plot_biplot_top15(1, 3, res.mca, ind_res))
print(plot_biplot_top15(2, 3, res.mca, ind_res))


# ==============================================================================
# ==============================================================================
# EXTRAS I GRÀFICS COMPLEMENTARIS (NO INCLOSOS A L'INFORME)
# ==============================================================================
# ==============================================================================

# A) Exploració univariant bàsica
# for (i in 1:length(vars_actives)) {
#   plot(df.active[, i], main = colnames(df.active)[i], ylab = "Count", col = "steelblue", las = 2)
# }

# B) Qualitat de la representació de categories (cos²)
# fviz_cos2(res.mca, choice = "var", axes = 1:3)

# C) Validació d'agrupacions (Clusters i Variables Exògenes)
# - Validació per clúster original
# fviz_mca_ind(res.mca, label = "none", habillage = as.factor(df$cluster),
#              palette = c("#00AFBB", "#E7B800", "#FC4E07"), addEllipses = TRUE,
#              ellipse.type = "confidence", ggtheme = theme_minimal()) +
#   labs(title = "Individus per cluster", subtitle = "Validació amb model anterior")

# - Validació per tipus d'habitació
# fviz_mca_ind(res.mca, label = "none", habillage = "room_type",
#              addEllipses = TRUE, ellipse.type = "confidence", ggtheme = theme_minimal())

# D) Projecció de variables suplementàries
# fviz_ellipses(res.mca, c("neighbourhood_cleansed", "property_type"), geom = "point") +
#   labs(title = "El·lipses de confiança – variables suplementàries")