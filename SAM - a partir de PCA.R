############ Spectral angle mapper (SAM) a partir del PCA ###########

## Carga de librerias ##
library("hsdar")
library(ggplot2)
library(RStoolbox)
library(sf)
library(raster)
library(caret)

############## LOTES C, E Y B #############

## Leer firmas espectrales de referencia ##

ref_hlb_ceb <- read.csv("ref_hlb_pca_ceb.csv", header = FALSE)

## Armar endmembers y reordenar tablas ##
  
endmembers_psa_ceb <- data.frame(t(ref_hlb_ceb[,-1]))
nombres_filas <- c("con_hlb", "sin_hlb")
row.names(endmembers_psa_ceb) <- nombres_filas

## Definir carpeta de trabajo y carga de imagen resultado del PCA ##

setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/chajari_mayo2023/prisma")
prisma_vis_nir_230_hlb_psa_ceb=brick("rpc_PRS_L2D_STD_20230530_recorte_lotes_ceb.tif")

## Cálculo SAM ##

prisma_sam_psa_ceb <- RStoolbox::sam(prisma_vis_nir_230_hlb_psa_ceb, endmembers_psa_ceb, angles = TRUE)
plot(prisma_sam_psa_ceb)

## Clasificación por ángulo mínimo ##

clas_sam_psa_ceb <- RStoolbox::sam(prisma_vis_nir_230_hlb_psa_ceb, endmembers_psa_ceb, angles = FALSE)

## Mostrar mapa clasificado ##

ggR(clas_sam_psa_ceb, forceCat = TRUE, geom_raster=TRUE) + 
  scale_fill_manual(values = c("red", "green", "red"), labels = c("con_hlb", "sin_hlb"))


############## LOTES A y D #############

## Leer firmas espectrales de referencia ##

ref_hlb_ad <- read.csv("ref_hlb_pca_ad.csv", header = FALSE)

## Armar endmembers y reordenar tablas ##

endmembers_psa_ad <- data.frame(t(ref_hlb_ad[,-1]))
nombres_filas <- c("con_hlb", "sin_hlb")
row.names(endmembers_psa_ad) <- nombres_filas

## Definir carpeta de trabajo y carga de imagen resultado del PCA ##

setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/chajari_mayo2023/prisma")
prisma_vis_nir_230_hlb_psa_ad=brick("rpc_PRS_L2D_STD_20230530_recorte_lotes_ad.tif")

## Cálculo SAM ##

prisma_sam_psa_ad <- RStoolbox::sam(prisma_vis_nir_230_hlb_psa_ad, endmembers_psa_ad, angles = TRUE)
plot(prisma_sam_psa_ad)

## Clasificación por ángulo mínimo ##

clas_sam_psa_ad <- RStoolbox::sam(prisma_vis_nir_230_hlb_psa_ad, endmembers_psa_ad, angles = FALSE)

## Mostrar mapa clasificado ##

ggR(clas_sam_psa_ad, forceCat = TRUE, geom_raster=TRUE) + 
  scale_fill_manual(values = c("red", "green", "red"), labels = c("con_hlb", "sin_hlb"))


### Exportar ambos mapas clasificados ###

#raster::writeRaster(x=clas_sam_psa_ad, filename ="clas_sam_psa_ad.tif", format ='GTiff', overwrite = TRUE)
#raster::writeRaster(x=clas_sam_psa_ceb, filename ="clas_sam_psa_ceb.tif", format ='GTiff', overwrite = TRUE)


############## Matriz de confusion CEB #############

## Recodificar raster clasificado en binario ##

clas_sam_psa_ceb_BI=clas_sam_psa_ceb
clas_sam_psa_ceb_BI[clas_sam_psa_ceb_BI<1.5]=1
clas_sam_psa_ceb_BI[clas_sam_psa_ceb_BI>=1.5]=0

## Visualizar ##
plot(clas_sam_psa_ceb_BI)
values(clas_sam_psa_ceb_BI)

## Ruta al archivo shapefile de puntos (validación) ##

ruta_shapefile <- "union_matrix_confusion.shp"
puntos_sf <- st_read(ruta_shapefile)

## Define imagen de referencia y vuelve a leer el shapefile ##

imagen_ceb <- clas_sam_psa_ceb_BI
puntos_sf <- st_read("./union_matrix_confusion.shp")

## Convierte geometría, reproyecta y extrae valores del raster ##

puntos_sf <-st_cast(puntos_sf,"POINT") %>% st_transform(crs = crs(imagen_ceb)) %>% st_as_sf()
datos_muestreadis <- terra::extract(imagen_ceb,(puntos_sf),bind=T)
puntos_sf$imagen_ceb <- datos_muestreadis

## Calcula la matriz de confusión ##

conf_matrix <- confusionMatrix(table(puntos_sf$imagen_ceb, puntos_sf$sintoma))

## Mostrar la matriz de confusión ##

print(conf_matrix)


############## Matriz de confusion AD ############# 

## Recodificar raster clasificado en binario ##

clas_sam_psa_ad_BI=clas_sam_psa_ad
clas_sam_psa_ad_BI[clas_sam_psa_ad_BI<1.5]=1
clas_sam_psa_ad_BI[clas_sam_psa_ad_BI>=1.5]=0

## Visualizar ##

plot(clas_sam_psa_ad_BI)
values(clas_sam_psa_ad_BI)

## Ruta al archivo shapefile de puntos (validación) ##

imagen_ad <- clas_sam_psa_ad_BI
puntos_sf <- st_read("./union_matrix_confusion.shp")

## Convierte geometría, reproyecta y extrae valores del raster ##

puntos_sf <-st_cast(puntos_sf,"POINT") %>% st_transform(crs = crs(imagen_ad)) %>% st_as_sf()
datos_muestreadis <- terra::extract(imagen_ad,(puntos_sf),bind=T)
puntos_sf$imagen_ad <- datos_muestreadis

## Calcula la matriz de confusión ##

conf_matrix <- confusionMatrix(table(puntos_sf$imagen_ad, puntos_sf$sintoma))

## Mostrar la matriz de confusión ##

print(conf_matrix)



