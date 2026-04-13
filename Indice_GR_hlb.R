# ============================================================
# Análisis del índice Green/Red sobre imágenes PRISMA
# ============================================================
# Este script:
# 1. Carga imágenes PRISMA recortadas
# 2. Calcula el índice Green/Red
# 3. Genera una clasificación binaria por umbrales
# 4. Evalúa la clasificación con puntos de referencia
# 5. Genera un boxplot del índice continuo
# ============================================================

# ---------------------------
# 1. Cargar librerías
# ---------------------------
library(raster)
library(sf)
library(terra)
library(caret)
library(magrittr)  # para usar %>%

# ---------------------------
# 2. Definir carpeta de trabajo
# ---------------------------
setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/chajari_mayo2023/prisma/")

# ---------------------------
# 3. Cargar shapefile de puntos de validación
# ---------------------------
ruta_shapefile <- "union_matrix_confusion_molecular.shp"
puntos_sf <- st_read(ruta_shapefile, quiet = TRUE)
puntos_sf <- st_cast(puntos_sf, "POINT")

# ============================================================
# BLOQUE 1. LOTES AD
# ============================================================

# ---------------------------
# 1.1 Cargar imagen PRISMA AD
# ---------------------------
ruta_ad <- "prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad.tif"
img_ad <- brick(ruta_ad)

# ---------------------------
# 1.2 Calcular índice Green/Red
# ---------------------------
# Índice calculado como:
# banda 21 / banda 34
indice_gr_ad <- img_ad[[21]] / img_ad[[34]]

# Visualizar índice
plot(indice_gr_ad, main = "Índice Green/Red - AD")

# Guardar índice
writeRaster(indice_gr_ad,
            filename = "indice_gr_ad.tif",
            format = "GTiff",
            overwrite = TRUE)

# ---------------------------
# 1.3 Clasificación binaria del índice
# ---------------------------
# Regla aplicada:
# < 1.40       -> 0
# 1.40 a 1.55  -> 1
# > 1.55       -> 0

clas_indice_gr_ad <- indice_gr_ad
clas_indice_gr_ad[clas_indice_gr_ad < 1.40] <- 0
clas_indice_gr_ad[(1.40 <= clas_indice_gr_ad) & (clas_indice_gr_ad <= 1.55)] <- 1
clas_indice_gr_ad[clas_indice_gr_ad > 1.55] <- 0

# Visualizar clasificación
plot(clas_indice_gr_ad, main = "Clasificación binaria índice GR - AD")

# Guardar clasificación
writeRaster(clas_indice_gr_ad,
            filename = "clas_indice_gr_ad_BI.tif",
            format = "GTiff",
            overwrite = TRUE)

# ---------------------------
# 1.4 Extraer clase predicha en puntos de muestreo
# ---------------------------
# Reproyectar puntos al sistema de coordenadas del raster
puntos_ad <- st_transform(puntos_sf, crs = crs(clas_indice_gr_ad))

# Convertir raster a formato terra
clas_indice_gr_ad_terra <- rast(clas_indice_gr_ad)

# Extraer valores del raster en cada punto
extraidos_ad <- terra::extract(clas_indice_gr_ad_terra, vect(puntos_ad))

# Guardar valores extraídos en el objeto de puntos
puntos_ad$imagen_ad <- extraidos_ad[[2]]

# ---------------------------
# 1.5 Calcular matriz de confusión
# ---------------------------
conf_matrix_ad <- confusionMatrix(
  table(Predicho = puntos_ad$imagen_ad,
        Referencia = puntos_ad$sintoma)
)

print(conf_matrix_ad)

# ---------------------------
# 1.6 Extraer valores continuos del índice para boxplot
# ---------------------------
indice_gr_ad_terra <- rast(indice_gr_ad)
extraidos_indice_ad <- terra::extract(indice_gr_ad_terra, vect(puntos_ad))
puntos_ad$indice_ad <- extraidos_indice_ad[[2]]

# Boxplot del índice continuo según síntoma
boxplot(indice_ad ~ sintoma,
        data = puntos_ad,
        col = c("blue", "red"),
        main = "Boxplot del índice Green/Red - AD",
        xlab = "Síntoma",
        ylab = "Valor del índice",
        names = c("Negativo", "Positivo"),
        notch = TRUE)

# ============================================================
# BLOQUE 2. LOTES CEB
# ============================================================

# ---------------------------
# 2.1 Cargar imagen PRISMA CEB
# ---------------------------
ruta_ceb <- "prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb.tif"
img_ceb <- brick(ruta_ceb)

# ---------------------------
# 2.2 Calcular índice Green/Red
# ---------------------------
# Índice calculado como:
# banda 21 / banda 34
indice_gr_ceb <- img_ceb[[21]] / img_ceb[[34]]

# Visualizar índice
plot(indice_gr_ceb, main = "Índice Green/Red - CEB")

# Guardar índice
writeRaster(indice_gr_ceb,
            filename = "indice_gr_ceb.tif",
            format = "GTiff",
            overwrite = TRUE)

# ---------------------------
# 2.3 Clasificación binaria del índice
# ---------------------------
# Regla aplicada:
# < 1.40       -> 0
# 1.40 a 1.55  -> 1
# > 1.55       -> 0

clas_indice_gr_ceb <- indice_gr_ceb
clas_indice_gr_ceb[clas_indice_gr_ceb < 1.40] <- 0
clas_indice_gr_ceb[(1.40 <= clas_indice_gr_ceb) & (clas_indice_gr_ceb <= 1.55)] <- 1
clas_indice_gr_ceb[clas_indice_gr_ceb > 1.55] <- 0

# Visualizar clasificación
plot(clas_indice_gr_ceb, main = "Clasificación binaria índice GR - CEB")

# Guardar clasificación
writeRaster(clas_indice_gr_ceb,
            filename = "clas_indice_gr_ceb_BI.tif",
            format = "GTiff",
            overwrite = TRUE)

# ---------------------------
# 2.4 Extraer clase predicha en puntos de muestreo
# ---------------------------
# Reproyectar puntos al sistema de coordenadas del raster
puntos_ceb <- st_transform(puntos_sf, crs = crs(clas_indice_gr_ceb))

# Convertir raster a formato terra
clas_indice_gr_ceb_terra <- rast(clas_indice_gr_ceb)

# Extraer valores del raster en cada punto
extraidos_ceb <- terra::extract(clas_indice_gr_ceb_terra, vect(puntos_ceb))

# Guardar valores extraídos en el objeto de puntos
puntos_ceb$imagen_ceb <- extraidos_ceb[[2]]

# ---------------------------
# 2.5 Calcular matriz de confusión
# ---------------------------
conf_matrix_ceb <- confusionMatrix(
  table(Predicho = puntos_ceb$imagen_ceb,
        Referencia = puntos_ceb$sintoma)
)

print(conf_matrix_ceb)

# ---------------------------
# 2.6 Extraer valores continuos del índice para boxplot
# ---------------------------
indice_gr_ceb_terra <- rast(indice_gr_ceb)
extraidos_indice_ceb <- terra::extract(indice_gr_ceb_terra, vect(puntos_ceb))
puntos_ceb$indice_ceb <- extraidos_indice_ceb[[2]]

# Boxplot del índice continuo según síntoma
boxplot(indice_ceb ~ sintoma,
        data = puntos_ceb,
        col = c("blue", "red"),
        main = "Boxplot del índice Green/Red - CEB",
        xlab = "Síntoma",
        ylab = "Valor del índice",
        names = c("Negativo", "Positivo"),
        notch = TRUE)



