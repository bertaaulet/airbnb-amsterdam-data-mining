# ==============================================================================
# MODEL GEOESTADÍSTIC TIPUS I - PREDICCIÓ DE PREUS (KRIGING ORDINARI)
# ==============================================================================

# ------------------------------------------------------------------------------
# LECTURA DE LLIBRERIES
# ------------------------------------------------------------------------------
library(sp)
library(gstat)
library(sf)
library(dplyr)
library(ggplot2)
library(geoR)

# ==============================================================================
# 1. PREPARACIÓ DE LES DADES I TRANSFORMACIÓ ESPACIAL
# ==============================================================================

# Carreguem el dataset i filtrem per la zona centre d'Amsterdam des de la carpeta data
df <- read.csv2("../data/dataset_powerbi_mapes.csv")

centro_ams <- subset(df,
                     longitude >= 4.800 & longitude <= 4.950 &
                       latitude >= 52.330 & latitude <= 52.410)

# Selecció d'una mostra aleatòria de 500 observacions
set.seed(123)
mostra_500 <- centro_ams[sample(nrow(centro_ams), 500), ]

# Projecció de les coordenades geogràfiques a UTM (Zona 31N)
dades_sf <- st_as_sf(
  mostra_500,
  coords = c("longitude", "latitude"),
  crs = 4326
)
dades_utm <- st_transform(dades_sf, crs = 32631)
coords_utm <- st_coordinates(dades_utm)

# Creació del dataframe geogràfic amb la variable resposta
df_geo <- data.frame(
  x = coords_utm[,1],
  y = coords_utm[,2],
  log_price = mostra_500$log_price
)

# Eliminació de registres amb duplicats espacials per evitar singularitats
df_geo <- df_geo[!duplicated(df_geo[, c("x", "y")]), ]
cat("Nombre de punts finals sense duplicats:", nrow(df_geo), "\n")

# Conversió a objecte espacial 'sp' i assignació del CRS
coordinates(df_geo) <- c("x", "y")
proj4string(df_geo) <- CRS("+proj=utm +zone=31 +datum=WGS84 +units=m")

# ==============================================================================
# 2. CREACIÓ DE LA MALLA DE PREDICCIÓ (GRID)
# ==============================================================================

# Generació d'una malla regular amb una resolució de 100 metres
grd <- expand.grid(
  x = seq(min(df_geo$x), max(df_geo$x), by = 100),
  y = seq(min(df_geo$y), max(df_geo$y), by = 100)
)

# Conversió de la malla a objecte espacial de tipus píxel
coordinates(grd) <- c("x", "y")
gridded(grd) <- TRUE
proj4string(grd) <- CRS("+proj=utm +zone=31 +datum=WGS84 +units=m")

# ==============================================================================
# 3. ANÀLISI EXPLORATÒRIA I SEMIVARIOGRAMA
# ==============================================================================

# Diagnòstic de la tendència espacial amb geoR
df_temporal <- data.frame(
  x = coordinates(df_geo)[,1],
  y = coordinates(df_geo)[,2],
  valor = df_geo$log_price
)

airbnb.geoR <- as.geodata(df_temporal, coords.col = 1:2, data.col = 3)
plot(airbnb.geoR, lowess = TRUE, scatter3d = FALSE, cex = 0.8)

# Càlcul del semivariograma empíric
ve <- variogram(
  log_price ~ 1,
  df_geo,
  cutoff = 1500,
  width = 100
)
print(ve)

plot(
  ve,
  plot.numbers = TRUE,
  pch = 19,
  col = "blue",
  main = "Semivariograma Empíric (log_price)",
  xlab = "Distància h (metres)",
  ylab = "Semivariància"
)

# Ajust automàtic del model teòric (Esfèric)
var_total <- var(df_geo$log_price)

fit_vgm <- fit.variogram(
  ve,
  model = vgm(
    psill = var_total * 0.05,
    model = "Sph",
    range = 800,
    nugget = var_total * 0.95
  ),
  fit.sills = TRUE,
  fit.ranges = TRUE,
  fit.method = 7
)

print("Paràmetres del model ajustat:")
print(fit_vgm)

plot(
  ve,
  fit_vgm,
  lwd = 2,
  col = "red",
  pch = 19,
  cex = 1.5,
  main = "Ajust Final del Variograma Teòric"
)

# ==============================================================================
# 4. INTERPOLACIÓ AMB KRIGING ORDINARI
# ==============================================================================

# Execució de l'algorisme de Kriging sobre la malla regular
ok <- krige(
  log_price ~ 1,
  locations = df_geo,
  newdata = grd,
  model = fit_vgm,
  nmax = 30
)

# Transformació inversa per recuperar els valors en euros
ok$pred_price_eur <- exp(ok$var1.pred)

print("Resum estadístic de les prediccions:")
summary(ok)

# ==============================================================================
# 5. VALIDACIÓ CREUADA DEL MODEL
# ==============================================================================

xvalid <- krige.cv(
  log_price ~ 1,
  df_geo,
  fit_vgm,
  nmax = 30,
  nfold = 5
)

# Càlcul dels indicadors de bondat d'ajust (R2 i RMSE)
r2_nfold <- cor(xvalid$observed, xvalid$var1.pred)^2
rmse_nfold <- sqrt(mean(xvalid$residual^2))

print(paste("R2 Validació Creuada:", round(r2_nfold, 4)))
print(paste("RMSE (escala log):", round(rmse_nfold, 4)))

# ==============================================================================
# 6. REPRESENTACIÓ GRÀFICA FINAL (COMPARATIVA DE COSTAT)
# ==============================================================================

# Configuració de la capa dels punts de la mostra original
pts.s <- list(
  "sp.points",
  df_geo,
  col = "black",
  pch = 20,
  cex = 0.4
)

# Definició de paletes de colors personalitzades (estil Viridis i Magma)
colors_prediccio <- colorRampPalette(c("#440154", "#31688e", "#35b779", "#fde725"))(100)
colors_variancia <- colorRampPalette(c("#000004", "#51127c", "#b63679", "#fb8861", "#fcfdbf"))(100)

# Generació del mapa de predicció de preus reals (Euros)
mapa_pred <- spplot(
  ok["pred_price_eur"],
  col.regions = colors_prediccio,
  scales = list(draw = TRUE),
  sp.layout = list(pts.s),
  main = "Predicció de Preus (€)"
)

# Generació del mapa de variància associada a l'error
mapa_var <- spplot(
  ok["var1.var"],
  col.regions = colors_variancia,
  scales = list(draw = TRUE),
  sp.layout = list(pts.s),
  main = "Variància de l'Error (Incertesa)"
)

# Dibuix dels dos mapes simultàniament un al costat de l'altre
print(mapa_pred, split = c(1, 1, 2, 1), more = TRUE)
print(mapa_var,  split = c(2, 1, 2, 1))

# Summary de control final
summary(ok)