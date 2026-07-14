# ==============================================================================
# MODEL GEOESTADÍSTIC TIPUS II - PROCESSOS PUNTUALS
# ==============================================================================

# ------------------------------------------------------------------------------
# LECTURA DE LLIBRERIES
# ------------------------------------------------------------------------------
library(raster)
library(spatstat)
library(sf)
library(OpenStreetMap)

# Configuració de l'entorn Java per a les dependències del paquet rJava
# Nota: Aquesta ruta s'haurà d'adaptar segons la màquina on s'executi el projecte
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-21") 
library(rJava)

# ==============================================================================
# 0. CÀRREGA I PREPARACIÓ DE DADES
# ==============================================================================
# Lectura del dataset original des de la carpeta unificada
df <- read.csv2("../data/dataset_powerbi_mapes.csv")

# Filtratge de l'espai d'estudi (centre d'Amsterdam) basat en coordenades geogràfiques
centro_ams <- subset(df, longitude >= 4.800 & longitude <= 4.950 &
                       latitude >= 52.330 & latitude <= 52.410)

# Emmagatzematge dels límits en graus per a la descàrrega posterior de cartografia base
xmin_ams_deg <- 4.800
xmax_ams_deg <- 4.950
ymin_ams_deg <- 52.330
ymax_ams_deg <- 52.410

# Conversió a objecte espacial (sf) amb sistema de referència geodèsic WGS84
dades_sf <- st_as_sf(centro_ams, coords = c("longitude", "latitude"), crs = 4326)

# Transformació a sistema de coordenades projectat UTM (EPSG: 32631) per a anàlisi mètrica
dades_utm <- st_transform(dades_sf, crs = 32631)

# Extracció de coordenades projectades per a l'anàlisi de processos de punts
coords_utm <- st_coordinates(dades_utm)
xy <- data.frame(x = coords_utm[,1], y = coords_utm[,2])
df1 <- data.frame(xy, price = centro_ams$price, host_type = as.factor(centro_ams$host_type))

# Actualització dels límits de l'espai d'estudi en metres
xmin_ams <- min(xy$x)
xmax_ams <- max(xy$x)
ymin_ams <- min(xy$y)
ymax_ams <- max(xy$y)

# --- INICIALITZACIÓ DELS PUNTS D'INTERÈS (CONTEXT URBÀ) ---
df_llocs <- data.frame(
  Nom.del.lloc = c("Casa d'Anna Frank", "Dam Square", "Rembrandtplein", 
                   "Heineken Experience", "Rijksmuseum", "Vondelpark", "Barri Vermell"),
  Categoria = c("Història", "Història", "Oci Nocturn", 
                "Turisme", "Cultura / Museus", "Natura / Oci", "Oci Nocturn"),
  Latitud = c(52.3752, 52.3731, 52.3660, 52.3578, 52.3600, 52.3615, 52.3725),
  Longitud = c(4.8840, 4.8926, 4.8997, 4.8917, 4.8852, 4.8770, 4.8967)
)

# Projectem els llocs d'interès a UTM per assegurar la coherència espacial
llocs_sf <- st_as_sf(df_llocs, coords = c("Longitud", "Latitud"), crs = 4326)
llocs_utm <- st_transform(llocs_sf, crs = 32631)
coords_llocs <- st_coordinates(llocs_utm)

df_llocs$x_utm <- coords_llocs[,1]
df_llocs$y_utm <- coords_llocs[,2]
df_llocs$Color <- c("magenta", "blue", "green3", "orange", "purple", "cyan", "red")


# ==============================================================================
# CAS 1: PROCÉS DE PUNTS PUR (Amb Jittering)
# ==============================================================================
print("--- INICIANT ANÀLISI CAS 1: PROCÉS PUR ---")

# Creació del procés de punts amb desplaçament aleatori (jittering) per gestionar coincidències
dataxy_raw <- ppp(xy$x, xy$y, xrange=c(xmin_ams, xmax_ams), yrange=c(ymin_ams, ymax_ams))
y <- rjitter(dataxy_raw, radius=15, retry=TRUE) 
unitname(y) <- c("metro", "metros")

# Estimació de l'amplada de banda òptima segons el mètode de Scott
sigma_opt <- bw.scott(y) 

# Càlcul de la intensitat mitjana (lambda) expressada en unitats km²
lambda_pur <- summary(y)$intensity * 1000000
print(paste("Lambda (Procés Pur - REALitat) pisos/Km2:", lambda_pur))

# Generació de gràfics de densitat amb estimador de Jones-Diggle
plot(density(y, sigma=sigma_opt, diggle=TRUE), main="1. Densitat REAL: Procés Pur")
contour(density(y, sigma=sigma_opt, diggle=TRUE), add=TRUE, col="white")
plot(y, add=TRUE, size=0.5)

# Anàlisi de la distribució dels punts
hist(xy$x, xlab="x (metres)", ylab="Freqüència", main="1. Histograma Longitud UTM", col="cadetblue3")
hist(xy$y, xlab="y (metres)", ylab="Freqüència", main="1. Histograma Latitud UTM", col="burlywood1")

# Anàlisi de quadrícules (Quadrat Count)
Q_pur <- quadratcount(y, nx = 5, ny = 5)
plot(Q_pur, main="1. Quadrícules i Intensitat (Pur)")
plot(y, add=TRUE, size=0.5)

# Visualització 3D del model de densitat
persp(density(y, sigma=sigma_opt, diggle=TRUE), theta=30, phi=30, col="lightblue", main="1. 3D Procés Pur")


# ==============================================================================
# CAS 2: MARCA CONTÍNUA (Preu)
# ==============================================================================
print("--- INICIANT ANÀLISI CAS 2: MARCA CONTÍNUA (PREU) ---")

# Procés marcat pel preu com a variable contínua
y_price_raw <- ppp(xy$x, xy$y, xrange=c(xmin_ams, xmax_ams), yrange=c(ymin_ams, ymax_ams), marks=df1$price)
y_price <- rjitter(y_price_raw, radius=15, retry=TRUE)
unitname(y_price) <- c("metro", "metros")

print(paste("Intensitat base calculada per al Preu:", summary(y_price)$intensity * 1000000))

# Visualització de la densitat ponderada per preus
plot(density(y_price, sigma=sigma_opt, diggle=TRUE), main="2. Densitat ponderada pel Preu")
contour(density(y_price, sigma=sigma_opt, diggle=TRUE), add=T, col="white")
plot(y_price, add=T, size=0.0008)

# Anàlisi de quadrícules ponderada
Q_price <- quadratcount(y_price, nx = 5, ny = 5)
plot(Q_price, main="2. Quadrícules (Preu)")
plot(y_price, add=TRUE, size=0.5, use.marks=FALSE)

# Visualització 3D del model de valor
persp(density(y_price, sigma=sigma_opt, diggle=TRUE), theta=30, phi=30, col="lightgreen", main="2. 3D Marca Preu")


# ==============================================================================
# ANÀLISI PER SEGREGACIÓ (Professionals vs Particulars)
# ==============================================================================
dividir <- split(df1, df1$host_type, drop = TRUE)

# ==============================================================================
# CAS 3: PROFESSIONALS
# ==============================================================================
print("--- INICIANT ANÀLISI CAS 3: PROFESSIONALS ---")

agencias <- dividir$"Professional"
y_agencias_raw <- ppp(agencias$x, agencias$y, xrange=c(xmin_ams, xmax_ams), yrange=c(ymin_ams, ymax_ams), marks=agencias$host_type)
y_agencias <- rjitter(y_agencias_raw, radius=15, retry=TRUE)
unitname(y_agencias) <- c("metro", "metros")

lambda_prof <- summary(y_agencias)$intensity * 1000000
print(paste("Lambda (Professionals) pisos/Km2:", lambda_prof))

plot(density(y_agencias, sigma=sigma_opt, diggle=TRUE), main="3. Densitat REAL: Professionals")
contour(density(y_agencias, sigma=sigma_opt, diggle=TRUE), add=TRUE, col="white")
plot(y_agencias, add=TRUE, size=0.5)

plot(as.im(y_agencias), main="3. Imatge del patró (Professionals)")
plot(y_agencias, add = TRUE)


# ==============================================================================
# CAS 4: PARTICULARS
# ==============================================================================
print("--- INICIANT ANÀLISI CAS 4: PARTICULARS ---")

particulares <- dividir$"Particular"
y_particulares_raw <- ppp(particulares$x, particulares$y, xrange=c(xmin_ams, xmax_ams), yrange=c(ymin_ams, ymax_ams), marks=particulares$host_type)
y_particulares <- rjitter(y_particulares_raw, radius=15, retry=TRUE)
unitname(y_particulares) <- c("metro", "metros")

lambda_part <- summary(y_particulares)$intensity * 1000000
print(paste("Lambda (Particulars) pisos/Km2:", lambda_part))

plot(density(y_particulares, sigma=sigma_opt, diggle=TRUE), main="4. Densitat REAL: Particulars")
contour(density(y_particulares, sigma=sigma_opt, diggle=TRUE), add=TRUE, col="white")
plot(y_particulares, add=TRUE, size=0.5)


# ==============================================================================
# 5. GRÀFICS COMPARATIUS PER A L'ANÀLISI DE RESULTATS
# ==============================================================================
# Comparativa directa d'intensitats espacialment segregades
par(mfrow=c(1,2))
plot(density(y_agencias, sigma=sigma_opt, diggle=TRUE), main="1. Saturació: Professionals")
contour(density(y_agencias, sigma=sigma_opt, diggle=TRUE), add=TRUE, col="white")

plot(density(y_particulares, sigma=sigma_opt, diggle=TRUE), main="2. Saturació: Particulars")
contour(density(y_particulares, sigma=sigma_opt, diggle=TRUE), add=TRUE, col="white")
par(mfrow=c(1,1))

# Gràfic de barres per contrastar les densitats mitjanes
valors_lambda <- c(lambda_prof, lambda_part)
noms_lambda <- c("Professionals (Agències)", "Particulars")
barplot(valors_lambda, names.arg = noms_lambda, col = c("thistle", "navajowhite"),
        main = "Comparativa d'Intensitat Espacial", ylab = "Densitat (Pisos per Km2)")


# ==============================================================================
# 6. CARTOGRAFIA INTEGRADA (MAPA BASE OSM)
# ==============================================================================
# Descàrrega del mapa base georeferenciat
mapa_base <- openmap(upperLeft = c(ymax_ams_deg, xmin_ams_deg), lowerRight = c(ymin_ams_deg, xmax_ams_deg), type="osm")
# Projectem la imatge base a UTM per alinear-la amb les dades
mapa_real_gris <- openproj(mapa_base, projection = "+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Normalització a escala de grisos per millorar la visualització de les densitats
for(i in 1:length(mapa_real_gris$tiles)) {
  colors_originals <- mapa_real_gris$tiles[[i]]$colorData
  rgb_mat <- col2rgb(colors_originals)
  grisos <- 0.299 * rgb_mat[1,] + 0.587 * rgb_mat[2,] + 0.114 * rgb_mat[3,]
  mapa_real_gris$tiles[[i]]$colorData[] <- rgb(grisos, grisos, grisos, maxColorValue = 255)
}

# Funció per a la visualització dels llocs emblemàtics
dibuixar_llegenda <- function() {
  points(df_llocs$x_utm, df_llocs$y_utm, pch=24, bg=df_llocs$Color, col="black", cex=1.5, lwd=2)
  legend("top", legend = df_llocs$Nom.del.lloc, pt.bg = df_llocs$Color, pch = 24, 
         col = "black", title = "Llocs Emblemàtics", ncol = 4, inset = 0.02, xpd = TRUE)
}

# --- Generació de mapes temàtics ---
plot(mapa_real_gris, main="Cartografia: Densitat Pur (Volum)")
densitat_pur <- density(y, sigma=sigma_opt, diggle=TRUE)
densitat_pur$v[densitat_pur$v < (max(densitat_pur$v, na.rm=TRUE) * 0.05)] <- NA
plot(densitat_pur, add=TRUE, col=adjustcolor(rev(heat.colors(256)), alpha.f = 0.50))
dibuixar_llegenda()


# ==============================================================================
# 7. ANÀLISI DE CONCENTRACIÓ VERTICAL (MULTI-LISTINGS)
# ==============================================================================
# Identificació d'edificis únics per analitzar l'impacte de les duplicitats
y_edificis <- unique(dataxy_raw) 
unitname(y_edificis) <- c("metro", "metros")

# Càlcul de la diferència per visualitzar el fenomen dels "hotels encoberts"
densitat_diferencia <- density(y, sigma=sigma_opt, diggle=TRUE) - density(y_edificis, sigma=sigma_opt, diggle=TRUE)

plot(mapa_real_gris, main="Heatmap: Concentració vertical (Multi-listings)")
densitat_dif_transp <- densitat_diferencia
densitat_dif_transp$v[densitat_dif_transp$v < (max(densitat_dif_transp$v, na.rm=TRUE) * 0.05)] <- NA
plot(densitat_dif_transp, add=TRUE, col=adjustcolor(rev(heat.colors(256)), alpha.f = 0.50))
dibuixar_llegenda()