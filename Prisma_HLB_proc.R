###############################################
# Lectura, conversión y armado de imagen PRISMA
# VNIR + SWIR (sin bandas solapadas)
###############################################

#----------------------------------------------
# 1. Cargar librerías necesarias
#----------------------------------------------
library(raster)       # Lectura y manipulación de imágenes raster multibanda
library(RStoolbox)    # Visualización RGB de imágenes satelitales
library(prismaread)   # Conversión de productos PRISMA (.he5) a GeoTIFF

#----------------------------------------------
# 2. Leer imágenes TIFF ya disponibles
#----------------------------------------------

# Definir directorio de trabajo con las imágenes de abril 2022
setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/imagen_202204")

# Ver archivos disponibles en la carpeta
list.files()

# Leer imagen multibanda original
ro <- brick("PRS_L2D_STD_20220402140222_20220402140226_0001.tif")

# Leer imagen multibanda modificada
ro1 <- brick("PRS_L2D_STD_20220402140222_20220402140226_0001_modificado.tif")

# Ver información general del objeto raster
ro1

#----------------------------------------------
# 3. Visualizar imágenes en composición RGB
#----------------------------------------------

# Mostrar imagen original en falso color usando bandas 34, 20 y 10
plotRGB(ro, r = 34, g = 20, b = 10, stretch = "hist")

# Mostrar la misma composición usando ggplot
ggRGB(ro, r = 34, g = 20, b = 10)

# Mostrar imagen modificada en la misma combinación RGB
plotRGB(ro1, r = 34, g = 20, b = 10, stretch = "hist")


#=========================================================
# 4. Conversión de archivo PRISMA .he5 a GeoTIFF
#=========================================================

# Definir directorio de trabajo con el producto PRISMA de mayo 2023
setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/chajari_mayo2023/prisma/")

# Listar archivos disponibles
list.files()

# Convertir el archivo PRISMA .he5 a distintos GeoTIFF
pr_convert(
  in_file    = "PRS_L2D_STD_20230530135541_20230530135545_0001.he5",
  out_folder = "prisma_hbl_out",
  overwrite  = TRUE,
  out_format = "GTiff",
  VNIR       = TRUE,   # Exporta bandas VNIR
  SWIR       = TRUE,   # Exporta bandas SWIR
  FULL       = TRUE,   # Exporta producto completo
  LATLON     = TRUE,   # Exporta capas de latitud/longitud
  ANGLES     = TRUE    # Exporta capas de ángulos
)

# Ver archivos generados en la carpeta de salida
list.files("prisma_hbl_out")


#=========================================================
# 5. Leer el producto VNIR exportado
#=========================================================

# Construir la ruta al archivo VNIR
prisma_vnir_path <- file.path(
  "prisma_hbl_out",
  "PRS_L2D_STD_20230530135541_20230530135545_0001_HCO_VNIR.tif"
)

# Leer el archivo VNIR como raster multibanda
prisma_vnir <- brick(prisma_vnir_path)

# Ver información general del raster
prisma_vnir

# Visualizar el VNIR en composición RGB
plotRGB(prisma_vnir, r = 34, g = 20, b = 10, stretch = "hist")


#=========================================================
# 6. Leer el producto SWIR exportado
#=========================================================

# Construir la ruta al archivo SWIR
prisma_swir_path <- file.path(
  "prisma_hbl_out",
  "PRS_L2D_STD_20230530135541_20230530135545_0001_HCO_SWIR.tif"
)

# Leer el archivo SWIR como raster multibanda
prisma_swir <- brick(prisma_swir_path)

# Visualizar el SWIR en composición RGB
plotRGB(prisma_swir, r = 40, g = 30, b = 20, stretch = "hist")


#=========================================================
# 7. Eliminar bandas solapadas del SWIR
#=========================================================

# Se eliminan las primeras 4 bandas del SWIR porque se solapan
# espectralmente con las últimas bandas del VNIR
prisma_swir_sin4 <- prisma_swir[[5:171]]


#=========================================================
# 8. Unir VNIR + SWIR sin bandas redundantes
#=========================================================

# Combinar el VNIR completo con el SWIR sin las primeras 4 bandas
prisma_vnir_swir_sin4 <- stack(prisma_vnir, prisma_swir_sin4)


#=========================================================
# 9. Exportar el stack final a GeoTIFF
#=========================================================

# Guardar la imagen combinada en formato GeoTIFF
writeRaster(
  x = prisma_vnir_swir_sin4,
  filename = "prisma_hbl_30_05_2023_vnir_swirsin4.tif",
  format = "GTiff",
  overwrite = TRUE
)

#----------------------------------------------
# 10. Verificación final
#----------------------------------------------

# Listar archivos disponibles
list.files("prisma_hbl_out")

# Visualizar el stack final combinado
plotRGB(prisma_vnir_swir_sin4, r = 34, g = 20, b = 10, stretch = "hist")

# Esta es la imagen final que luego se puede abrir en QGIS
# para revisar su ubicación espacial o georreferenciarla
