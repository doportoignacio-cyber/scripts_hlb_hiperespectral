library("raster")

setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/chajari_mayo2023/prisma/")
list.files()


########genero ruta para armar brick - lotes ceb

prisma_ceb_path <- file.path(
  "prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb.tif"
)
###########armo brick


prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb <- raster::brick(prisma_ceb_path)

prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb

norm_440_azul_ceb = prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb/prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb[[5]]

indice_hlb_ceb = norm_440_azul_ceb[[58]]/norm_440_azul_ceb[[34]]

plot(indice_hlb_ceb)

#raster::writeRaster(x=indice_hlb_ceb, filename ="indice_hlb_ceb.tif", format ='GTiff', overwrite = TRUE)


########genero ruta para armar brick - lotes ad

prisma_ad_path <- file.path(
  "prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad.tif"
)
###########armo brick


prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad<- raster::brick(prisma_ad_path)

prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad

norm_440_azul_ad = prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad/prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad[[5]]

indice_hlb_ad = norm_440_azul_ad[[58]] / norm_440_azul_ad[[34]]

plot(indice_hlb_ad)

#raster::writeRaster(x=indice_hlb_ad, filename ="indice_hlb_ad.tif", format ='GTiff', overwrite = TRUE)

hist(indice_hlb_ad, main = "Histograma del índice HLB AD",
     xlab = "Índice HLB", ylab = "Frecuencia", col = "skyblue", border = "white")

hist(indice_hlb_ceb, main = "Histograma del índice HLB CEB",
     xlab = "Índice HLB", ylab = "Frecuencia", col = "skyblue", border = "white")


#### Indice Green/Red

indice_gr_ad = prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad[[21]]/
  prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad[[34]]
plot(indice_gr_ad)  
#raster::writeRaster(x=indice_gr_ad, filename ="indice_gr_ad.tif", format ='GTiff', overwrite = TRUE)


indice_gr_ceb = prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb[[21]]/
  prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb[[34]]
plot(indice_gr_ceb)
#raster::writeRaster(x=indice_gr_ceb, filename ="indice_gr_ceb.tif", format ='GTiff', overwrite = TRUE)


#### Indice normalizado entre max NIR y max Green

indice_max_nir_green_ad = (prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad[[58]]-
  prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad[[21]])/
  (prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad[[58]]+
     prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad[[21]])
plot(indice_max_nir_green_ad)
raster::writeRaster(x=indice_max_nir_green_ad, filename ="indice_max_nir_green_ad.tif", format ='GTiff', overwrite = TRUE)


indice_max_nir_green_ceb = (prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb[[58]]-
                             prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb[[21]])/
  (prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb[[58]]+
     prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb[[21]])
plot(indice_max_nir_green_ceb)
#raster::writeRaster(x=indice_max_nir_green_ceb, filename ="indice_max_nir_green_ceb.tif", format ='GTiff', overwrite = TRUE)


#### Matriz de confusión para el indice gr

#### Matriz de confusion ad

clas_indice_gr_ad_BI=indice_gr_ad
clas_indice_gr_ad_BI[clas_indice_gr_ad_BI<1.4]=0
clas_indice_gr_ad_BI[(1.4 <= clas_indice_gr_ad_BI) & (clas_indice_gr_ad_BI <= 1.55)] = 1
clas_indice_gr_ad_BI[clas_indice_gr_ad_BI>1.55]=0


plot(clas_indice_gr_ad_BI)
values(clas_indice_gr_ad_BI)

raster::writeRaster(x=clas_indice_gr_ad_BI, filename ="clas_indice_gr_ad_BI.tif", format ='GTiff', overwrite = TRUE)


# Carga las bibliotecas
library(sf)
library(raster)

# Ruta al archivo shapefile de puntos
ruta_shapefile <- "union_matrix_confusion_molecular.shp"

# Carga el archivo shapefile
puntos_sf <- st_read(ruta_shapefile)

# Carga la imagen raster
#ruta_imagen <- "ruta/a/tu/imagen.tif"
imagen_ad <- clas_indice_gr_ad_BI
puntos_sf <- st_read("./union_matrix_confusion_molecular.shp")
puntos_sf <-st_cast(puntos_sf,"POINT") %>% st_transform(crs = crs(imagen_ad)) %>% st_as_sf()
datos_muestreadis <- terra::extract(imagen_ad,(puntos_sf),bind=T)
puntos_sf$imagen_ad <- datos_muestreadis


#mas bonita

#Cargar la librería
library(caret)

# Crear la matriz de confusión
conf_matrix <- confusionMatrix(table(puntos_sf$imagen_ad, puntos_sf$sintoma))

# Mostrar la matriz de confusión
print(conf_matrix)


#### Matriz de confusion ceb

clas_indice_gr_ceb_BI=indice_gr_ceb
clas_indice_gr_ceb_BI[clas_indice_gr_ceb_BI<1.4]=0
clas_indice_gr_ceb_BI[(1.4 <= clas_indice_gr_ceb_BI) & (clas_indice_gr_ceb_BI <= 1.55)] = 1
clas_indice_gr_ceb_BI[clas_indice_gr_ceb_BI>1.55]=0


plot(clas_indice_gr_ceb_BI)
values(clas_indice_gr_ceb_BI)

raster::writeRaster(x=clas_indice_gr_ceb_BI, filename ="clas_indice_gr_ceb_BI.tif", format ='GTiff', overwrite = TRUE)


# Ruta al archivo shapefile de puntos
ruta_shapefile <- "union_matrix_confusion_molecular.shp"

# Carga el archivo shapefile
puntos_sf <- st_read(ruta_shapefile)

# Carga la imagen raster
#ruta_imagen <- "ruta/a/tu/imagen.tif"
imagen_ceb <- clas_indice_gr_ceb_BI
puntos_sf <- st_read("./union_matrix_confusion_molecular.shp")
puntos_sf <-st_cast(puntos_sf,"POINT") %>% st_transform(crs = crs(imagen_ceb)) %>% st_as_sf()
datos_muestreadis <- terra::extract(imagen_ceb,(puntos_sf),bind=T)
puntos_sf$imagen_ceb <- datos_muestreadis


#mas bonita


# Crear la matriz de confusión
conf_matrix_ceb <- confusionMatrix(table(puntos_sf$imagen_ceb, puntos_sf$sintoma))

# Mostrar la matriz de confusión
print(conf_matrix_ceb)



##### Box Plot para muestras y valores en el índice

indice_gr_ceb
values(indice_gr_ceb)

# Ruta al archivo shapefile de puntos
ruta_shapefile <- "union_matrix_confusion_molecular.shp"

# Carga el archivo shapefile
puntos_sf <- st_read(ruta_shapefile)

# Carga la imagen raster
#ruta_imagen <- "ruta/a/tu/imagen.tif"
imagen_indice_gr_ceb <- indice_gr_ceb
puntos_sf <- st_read("./union_matrix_confusion_molecular.shp")
puntos_sf <-st_cast(puntos_sf,"POINT") %>% st_transform(crs = crs(imagen_indice_gr_ceb)) %>% st_as_sf()
datos_muestreadis <- terra::extract(imagen_indice_gr_ceb,(puntos_sf),bind=T)
puntos_sf$indice_ceb <- datos_muestreadis


library(sf)

# Crear el boxplot
# Crear un boxplot
boxplot(indice_ceb ~ sintoma, data = puntos_sf, col = c("blue", "red"), main = "Boxplot de indice_ceb por sintoma", xlab = "Sintoma", ylab = "Indice CEB", names=c("Negativo","Positivo"), notch=TRUE)


############
install.packages("remotes")
library(remotes)



