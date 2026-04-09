library("ggplot2")
library("caret")
#install.packages("pheatmap")
library("pheatmap")
#install.packages("MLmetrics")
library("MLmetrics")
library( "SDMTools")
library("rasterVis")
library("plot3D")
library("InformationValue")
library("ISLR")
library("rgdal")
library(sprawl)
library(sprawl.data)
library(raster)
library(readr)
library(readxl)
library(sf)
# for datasets
library(maps)
library(spData)
# for creating animations
library(magick)
# for plotting
library(grid)
library(tmap)
library(viridis)

#install.packages("devtools")
#devtools::install_github("lbusett/sprawl")
#lib

library(RStoolbox)
help(coregisterImages)

#install.packages("RStoolbox")
library("RStoolbox")

library("rgdal")
#install.packages("remotes")
library("remotes")
#remotes::install_github("lbusett/prismaread")
library("prismaread")
#install.packages("sp")
library("raster")
#install.packages("rjson")
library("rjson")


setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/imagen_202204")
list.files()

ro=brick("PRS_L2D_STD_20220402140222_20220402140226_0001.tif")
ro1=brick("PRS_L2D_STD_20220402140222_20220402140226_0001_modificado.tif")
ro1
plotRGB(ro, 34, 20, 10, stretch='hist')
ggRGB(ro, 34,20,10)

plotRGB(ro1, 34, 20, 10, stretch='hist')


####################### OTRA FORMA DE LEER CON PRISMA READ EL HDF (HE5)

setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/chajari_mayo2023/prisma/")
list.files()

pr_convert(
  in_file = "PRS_L2D_STD_20230530135541_20230530135545_0001.he5",
  out_folder = "prisma_hbl_out",
  overwrite=TRUE,
  out_format = "GTiff",
  VNIR = TRUE,
  SWIR = TRUE,
  FULL = TRUE,
  LATLON = TRUE,
  ANGLES = TRUE
)

list.files("prisma_hbl_out")

########genero ruta para armar brick

prisma_sr_vnir_path <- file.path(
  "prisma_hbl_out",
    "PRS_L2D_STD_20230530135541_20230530135545_0001_HCO_VNIR.tif"
)
###########armo brick


prisma_hbl_30_05_2023_vnir <- raster::brick(prisma_sr_vnir_path)
prisma_hbl_30_05_2023_vnir


#crs(prisma_sr_vnir)=c("+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs")
#crs(prisma_sr_vnir)=c("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

plotRGB(prisma_hbl_30_05_2023_vnir, 34, 20, 10, stretch='hist')
#Exporto a Geotif, se guarda en el directorio de trabajo
#writeRaster(x=prisma_sr_vnir, filename ="PRS_L2D_STD_20220311142257_20220311142301_0001_HCO_VNIR", format ='GTiff', overwrite = TRUE)

########################################SWIR

list.files("prisma_hbl_out")
prisma_sr_swir_path <- file.path(
  "prisma_hbl_out",
  "PRS_L2D_STD_20230530135541_20230530135545_0001_HCO_SWIR.tif" 
)
###########armo brick

prisma_hbl_30_05_2023_swir <- raster::brick(prisma_sr_swir_path)

plotRGB(prisma_hbl_30_05_2023_swir, 40, 30, 20, stretch='hist')

#Exporto a Geotif, se guarda en el directorio de trabajo
#writeRaster(x=prisma_hbl_02_04_2022_swir, filename ="prisma_hbl_02_04_2022_swir", format ='GTiff', overwrite = TRUE)


#Hago brick de prisma sin las cuatro primeras bandas del SWIR porque se solapan con las  últimas del VNIR

prisma_hbl_30_05_2023_swir_sin4=prisma_hbl_30_05_2023_swir[[5:171]]

####UNO LOS STACKS DE VNIR Y DE SWIR
prisma_hbl_30_05_2023_vnir_swirsin4=stack(prisma_hbl_30_05_2023_vnir,prisma_hbl_30_05_2023_swir_sin4)

########EXPORTO ESTE STACK COMPLETO VIS-SWIR SIN LAS 4 PRIMERAS BANDAS DEL SWIR A GEOTIF PARA ABRIR EN QGIS Y GEORREFERENCIAR
writeRaster(x=prisma_hbl_30_05_2023_vnir_swirsin4, filename ="prisma_hbl_30_05_2023_vnir_swirsin4.tif", format ='GTiff', overwrite = TRUE)

list.files("prisma_hbl_out")

plotRGB(prisma_hbl_30_05_2023_vnir_swirsin4, 34, 20, 10, stretch='hist')


# Esta imagen que guardo es la que deberia Georreferenciar en Qgis


