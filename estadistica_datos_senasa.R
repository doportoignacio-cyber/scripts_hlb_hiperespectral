
library(dplyr)
library(ggplot2)

setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/senasa")

datos <- read.csv("Datos_Federacion.csv")
datos$result <- as.numeric(datos$Resultado.Monitoreo == "Positivo")

sum(is.na(datos$Resultado.Monitoreo)) #no hay datos faltantes

misFechas <- do.call(rbind, strsplit(datos$Fecha.de.Muestra, "/"))

colnames(misFechas) <- c("dia", "mes", "anio") #doy nombre a las columnas

misFechas <- apply(misFechas, 2, as.numeric) # paso los datos a numericos

datos <- cbind(datos, misFechas) # agrego mis fechas como columnas al dataframe

datos_resumen <-
  datos %>% 
  group_by(Partido_Monitoreo, Especies, anio) %>% 
  summarise(
    prop = mean(result),
    Positivos = sum(result),
    n = n()
  )

# gráfico de barras con 2 variables
ggplot(datos_resumen, aes(x = anio)) +
  geom_bar(aes(y = n, fill = "N muestreado"), position = "dodge", stat = "identity") +
  geom_bar(aes(y = Positivos, fill = "Positivos"), position = "dodge", stat = "identity") +
  labs(title = "", x = "Años", y = "Valor", fill = "") +
  scale_fill_manual(values = c("N muestreado" = "orange", "Positivos" = "red"))





ggplot(datos_resumen, aes(anio, prop, 
                          color = Especies, 
                          group = Especies)) +
  geom_line() +
  labs(title = "Proporción de casos Positivos por especie anual", x = "Años", y = "% de Positivos", fill = "")
  



ggplot(datos_resumen, aes(anio, prop, 
                          fill = Especies, 
                          group = Especies)) +
  geom_bar(stat = "identity", position = "dodge")

ggplot(datos_resumen, aes(anio, prop, 
                          fill = Partido_Monitoreo, 
                          group = Partido_Monitoreo)) +
  labs(x = "Años", y = "Proporción Positivos", fill = "") +
  geom_bar(stat = "identity", position = "dodge")

ggplot(datos_resumen, aes(anio, n, 
                          fill = Partido_Monitoreo, 
                          group = Partido_Monitoreo)) +
  geom_bar(stat = "identity", position = "dodge")

#Analisis por Localidad

datos_resumen_localidad <-
  datos %>% 
  group_by(Localidad_Monitoreo, Especies, anio) %>% 
  summarise(
    prop = mean(result),
    Positivos = sum(result),
    n = n()
  )

# gráfico de barras con 2 variables
plot_localidades_all <- ggplot(datos_resumen_localidad, aes(x = Localidad_Monitoreo)) +
  geom_bar(aes(y = n, fill = "N muestreado"), position = "dodge", stat = "identity") +
  geom_bar(aes(y = Positivos, fill = "Positivos"), position = "dodge", stat = "identity") +
  labs(title = "", x = "Localidad", y = "Valor", fill = "") +
  scale_fill_manual(values = c("N muestreado" = "orange", "Positivos" = "red")) +
  theme(plot.title = element_text(size = 15),axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

ggsave(filename = "C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/Gráficos Estadisticos/Grafico_localidades.png", 
       plot = plot_localidades_all, width = 30, height = 20, dpi = 300, units = "cm")

ggplot(datos_resumen_localidad, aes(x = Localidad_Monitoreo)) +
  geom_bar(aes(y = n, fill = "N muestreado"), position = "dodge", stat = "identity") +
  geom_bar(aes(y = Positivos, fill = "Positivos"), position = "dodge", stat = "identity") +
  labs(title = "", x = "Localidad", y = "Valor", fill = "") +
  scale_fill_manual(values = c("N muestreado" = "orange", "Positivos" = "red")) +
  theme(plot.title = element_text(size = 15),axis.text.x = element_text(angle = 45, hjust = 1, size = 10))



datos_resumen <-
  datos %>% 
  group_by(Partido_Monitoreo, Localidad_Monitoreo, Especies, anio) %>% 
  summarise(
    prop = mean(result),
    n = n()
  )



ggplot(datos_resumen, aes(anio, n, 
                          color = Partido_Monitoreo, 
                          group = Partido_Monitoreo)) +
  geom_line() +
  facet_wrap(Especies ~ .)

ggplot(datos_resumen, aes(anio, n, 
                          color = Partido_Monitoreo, 
                          group = Partido_Monitoreo)) +
  geom_line() +
  facet_wrap(Localidad_Monitoreo ~ .)


datos2022_positivos <- subset(datos, anio == 2022 & Resultado.Monitoreo == "Positivo")
datos2022_negativos <- subset(datos, anio == 2022 & Resultado.Monitoreo == "Negativo")

# write.csv(datos2022_negativos, "datos2022_negativos.csv", row.names = FALSE)


datos2022 <- read.csv("datos2022.csv")

datos2022$result2022 <- as.numeric(datos2022$Resultado.Monitoreo == "Positivo")

sum(is.na(datos2022$Resultado.Monitoreo)) #no hay datos faltantes

misFechas2022 <- do.call(rbind, strsplit(datos2022$Fecha.de.Muestra, "/"))

colnames(misFechas2022) <- c("dia", "mes", "anio") #doy nombre a las columnas

misFechas2022 <- apply(misFechas, 2, as.numeric) # paso los datos a numericos

datos2022 <- cbind(datos2022, misFechas2022) # agrego mis fechas como columnas al dataframe

datos_resumen2022 <-
  datos2022 %>% 
  group_by(Partido_Monitoreo, anio, mes) %>% 
  summarise(
    prop = mean(result2022),
    Positivos_2022 = sum(result2022),
    n = n()
  )

ggplot(datos_resumen2022, aes(factor(mes, labels = month.name), prop, 
                          color = Localidad_Monitoreo, 
                          group = Localidad_Monitoreo)) +
  geom_line()

ggplot(datos_resumen2022, aes(factor(mes, labels = month.name), Positivos_2022, 
                              fill = Localidad_Monitoreo, 
                              group = Localidad_Monitoreo)) +
  geom_bar(stat = "identity", position = "dodge")

ggplot(datos_resumen2022, aes(factor(mes, labels = month.name), Positivos_2022, 
                          fill = Especies, 
                          group = Especies)) +
  geom_bar(stat = "identity", position = "dodge")

ggplot(datos_resumen2022, aes(factor(mes, labels = month.name), prop, 
                          fill = Partido_Monitoreo, 
                          group = Partido_Monitoreo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Meses", y = "Proporción Positivos", fill = "") +
  theme(plot.title = element_text(size = 15),axis.text.x = element_text(angle = 45, hjust = 1, size = 10))





# gráfico de barras con 2 variables
ggplot(datos_resumen2022, aes(factor(mes, labels = month.name))) +
  geom_bar(aes(y = n, fill = "N muestreado"), position = "dodge", stat = "identity") +
  geom_bar(aes(y = Positivos_2022, fill = "Positivos"), position = "dodge", stat = "identity") +
  labs(title = "N muestreado y casos positivos de HLB para el año 2022", x = "Meses", y = "Valor", fill = "") +
  scale_fill_manual(values = c("N muestreado" = "orange", "Positivos" = "red"))



#Analisis por Localidad

datos_resumen_localidad_2022 <-
  datos2022 %>% 
  group_by(Localidad_Monitoreo, Especies, anio) %>% 
  summarise(
    prop = mean(result),
    Positivos = sum(result),
    n = n()
  )

# gráfico de barras con 2 variables
plot_localidades <- ggplot(datos_resumen_localidad_2022, aes(x = Localidad_Monitoreo)) +
  geom_bar(aes(y = n, fill = "N muestreado"), position = "dodge", stat = "identity") +
  geom_bar(aes(y = Positivos, fill = "Positivos"), position = "dodge", stat = "identity") +
  labs(title = "N muestreado y casos Positivos de HLB por Localidad - Año 2022", x = "Localidad", y = "Valor", fill = "") +
  scale_fill_manual(values = c("N muestreado" = "orange", "Positivos" = "red")) +
  theme(plot.title = element_text(size = 15),axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

ggsave(filename = "C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/Gráficos Estadisticos/Grafico_localidades.png", 
       plot = plot_localidades, width = 30, height = 20, dpi = 300, units = "cm")

ggplot(datos_resumen_localidad_2022, aes(x = Localidad_Monitoreo)) +
  geom_bar(aes(y = n, fill = "N muestreado"), position = "dodge", stat = "identity") +
  geom_bar(aes(y = Positivos, fill = "Positivos"), position = "dodge", stat = "identity") +
  labs(title = "N muestreado y casos Positivos de HLB por Localidad - Año 2022", x = "Localidad", y = "Valor", fill = "") +
  scale_fill_manual(values = c("N muestreado" = "orange", "Positivos" = "red")) +
  theme(plot.title = element_text(size = 15),axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
