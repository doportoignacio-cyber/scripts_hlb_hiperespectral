
#devtools::install_github("bleutner/RStoolbox")


library("RStoolbox")
library("remotes")
#install.packages("rgdal")
library("rgdal")
library("prismaread")
library("raster")
library("rjson")
#install.packages("signal")
#install.packages("Boruta")
#install.packages("hsdar")
library("hsdar")



setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/chajari_mayo2023/prisma")

###########################################Calculo PCA

set.seed(1626696) ### para garantizar que sea reproducible

#prisma_sr2022_vnir_swirsin4_lago_recorte=crop(prisma_sr2022_vnir_swirsin4, lago_completo)
#prisma_sr2022_vnir_swirsin4_lago=raster::mask(x =prisma_sr2022_vnir_swirsin4_lago_recorte,mask=lago_completo)


#####################Probamos con imagen recortada  LOTES AyD Ignacio/PRISMA


PRS_L2D_STD_20230530_recorte_lotes_ad=brick("prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ad.tif")
ggRGB(PRS_L2D_STD_20230530_recorte_lotes_ad, 34,20,10, stretch = "lin")


#######################Aca viene la parte de calcular componentes principales
rpc_PRS_L2D_STD_20230530_recorte_lotes_ad <- rasterPCA(PRS_L2D_STD_20230530_recorte_lotes_ad)
#raster::writeRaster(x=rpc_PRS_L2D_STD_20230530_recorte_lotes_ad$map, filename ="rpc_PRS_L2D_STD_20230530_recorte_lotes_ad.tif", format ='GTiff', overwrite = TRUE)
#raster::writeRaster(x=rpc_PRS_L2D_STD_20230530_recorte_lotes_ad$map[[1]], filename ="CP1_PRS_L2D_STD_20230530_recorte_lotes_ad.tif", format ='GTiff', overwrite = TRUE)


sum_cpa_lotes_ad=summary(rpc_PRS_L2D_STD_20230530_recorte_lotes_ad$model)
sum_loadings_cpa_lotes_ad=loadings(rpc_PRS_L2D_STD_20230530_recorte_lotes_ad$model)
proporcion_explicada_ad<- round((sum_cpa_lotes_ad$sdev^2)/sum(sum_cpa_lotes_ad$sdev^2)*100,3)
proporcion_explicada_ad <- data.frame(proporcion_explicada_ad)
proporcion_explicada_ad$Componente <- (1:nrow(proporcion_explicada_ad))
ggplot(proporcion_explicada_ad[1:10,],aes(y=proporcion_explicada_ad,x=Componente))+
  geom_col() + 
  labs(y = "Proporción explicada Lotes AD", x = "Componentes")

ggRGB(rpc_PRS_L2D_STD_20230530_recorte_lotes_ad$map, 1,2,3, stretch = "lin")
plot(rpc_PRS_L2D_STD_20230530_recorte_lotes_ad$map[[1]])


#####################Probamos con imagen recortada  LOTES CEB Ignacio/PRISMA


PRS_L2D_STD_20230530_recorte_lotes_ceb=brick("prisma_hbl_30_05_2023_vnir_swirsin4_modified_recorte_lotes_ceb.tif")
ggRGB(PRS_L2D_STD_20230530_recorte_lotes_ceb, 34,20,10, stretch = "lin")


#######################Aca viene la parte de calcular componentes principales
rpc_PRS_L2D_STD_20230530_recorte_lotes_ceb <- rasterPCA(PRS_L2D_STD_20230530_recorte_lotes_ceb)
#raster::writeRaster(x=rpc_PRS_L2D_STD_20230530_recorte_lotes_ceb$map, filename ="rpc_PRS_L2D_STD_20230530_recorte_lotes_ceb.tif", format ='GTiff', overwrite = TRUE)
#raster::writeRaster(x=rpc_PRS_L2D_STD_20230530_recorte_lotes_ceb$map[[2]], filename ="CP2_PRS_L2D_STD_20230530_recorte_lotes_ceb.tif", format ='GTiff', overwrite = TRUE)


sum_cpa_lotes_ceb=summary(rpc_PRS_L2D_STD_20230530_recorte_lotes_ceb$model)
sum_loadings_cpa_lotes_ceb=loadings(rpc_PRS_L2D_STD_20230530_recorte_lotes_ceb$model)
proporcion_explicada_ceb<- round((sum_cpa_lotes_ceb$sdev^2)/sum(sum_cpa_lotes_ceb$sdev^2)*100,3)
proporcion_explicada_ceb <- data.frame(proporcion_explicada_ceb)
proporcion_explicada_ceb$Componente <- (1:nrow(proporcion_explicada_ceb))
ggplot(proporcion_explicada_ceb[1:10,],aes(y=proporcion_explicada_ceb,x=Componente))+
  geom_col() + 
  labs(y = "Proporción explicada Lotes CEB", x = "Componentes")


ggRGB(rpc_PRS_L2D_STD_20230530_recorte_lotes_ceb$map, 1,2,3, stretch = "lin")
plot(rpc_PRS_L2D_STD_20230530_recorte_lotes_ceb$map[[2]])


#PARA GRAFICAR DIFERENTES COMPONENTES Y PODES COMPARAR

# Extraer las primeras nueve componentes principales
primeras_nueve_cp <- rpc_PRS_L2D_STD_20230530_recorte_lotes_ceb$map[[1:4]]

# Graficar las primeras nueve componentes principales
par(mfrow=c(2, 3)) # Divide la ventana gráfica en 2 filas y 2 columnas

for (i in 1:4) {
  plot(primeras_nueve_cp[[i]], main = paste("Componente Principal", i))
}


# Cargar bibliotecas
library(cluster)


raster_stack<- primeras_nueve_cp
#Convertir el stack a un formato adecuado para clustering


stack_values <- getValues(raster_stack)
#stack_values <- t(stack_values)

# Realizar clustering con k-means (reemplaza 'k' con el número deseado de clústeres)
k <- 4
kmeans_result <- kmeans((stack_values), 
                        centers = k)

# Asignar las etiquetas de clúster a cada píxel
cluster_labels <- kmeans_result$cluster
cluster_raster <- raster_stack[[1]]
cluster_raster[] <- cluster_labels
extent(cluster_raster) <- extent(raster_stack)

# Plotear el resultado
plot(cluster_raster, main = "Clasificación no supervisada", col = terrain.colors(k), axes = FALSE, legend = TRUE)
raster::writeRaster(x=cluster_raster, filename ="clas_no_sup_CP1a4.tif", format ='GTiff', overwrite = TRUE)

# Agregar etiquetas de ejes
axis(1, at = seq(min(raster_stack), max(raster_stack), length.out = 5), labels = TRUE)
axis(2, at = seq(min(raster_stack), max(raster_stack), length.out = 5), labels = TRUE)

# Mostrar leyenda
legend("topright", legend = 1:k, fill = terrain.colors(k), title = "Clúster")




########################Dibujo porcentaje de varianzas

cv_pca_lago=read.table("summary_lake_pca_11_03_2022_cv", sep=",", header=FALSE)
sd=cv_pca_lago_t=t(cv_pca_lago)
cv_pca_lago_t=cv_pca_lago_t[1:230]
cv_pca_lago_t
x=c(1:230)

#cv_pca_lago_t[1:7] 0.9498406 0.9680287 0.9762113 0.9793499 0.9819161 0.9835720 0.9851095 0.9864140 0.9874867
#50 0.996
base::plot(x,cv_pca_lago_t, xlab="Dimensions",ylab="Explained Variance", main="Explained variance as a function of the number of dimensions")
abline(v=5, col=2)
legend('bottomright', "PC5", cex=0.8, col=2,lty=1)



