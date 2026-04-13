# =========================================================
# ANALISIS DE DATOS DE MONITOREO HLB
# =========================================================
# Este script:
# 1. Lee la base de datos general
# 2. Crea una variable binaria para resultado positivo
# 3. Separa la fecha en dia, mes y anio
# 4. Resume la informacion por partido, localidad, especie y tiempo
# 5. Genera graficos generales y para el año 2022
# =========================================================


# =========================================================
# 1. CARGA DE LIBRERIAS
# =========================================================
library(dplyr)
library(ggplot2)


# =========================================================
# 2. DEFINICION DEL DIRECTORIO DE TRABAJO
# =========================================================
setwd("C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/datos/senasa")


# =========================================================
# 3. LECTURA DE LA BASE GENERAL
# =========================================================
datos <- read.csv("Datos_Federacion.csv")


# =========================================================
# 4. PREPARACION DE VARIABLES
# =========================================================

# Crear variable binaria:
# 1 = Positivo
# 0 = No positivo
datos$result <- as.numeric(datos$Resultado.Monitoreo == "Positivo")

# Verificar faltantes en la variable de resultado
sum(is.na(datos$Resultado.Monitoreo))  # no hay datos faltantes

# Separar la fecha en dia, mes y anio
misFechas <- do.call(rbind, strsplit(datos$Fecha.de.Muestra, "/"))
colnames(misFechas) <- c("dia", "mes", "anio")

# Convertir a numerico
misFechas <- apply(misFechas, 2, as.numeric)

# Agregar las nuevas columnas al data frame original
datos <- cbind(datos, misFechas)


# =========================================================
# 5. RESUMEN GENERAL POR PARTIDO, ESPECIE Y AÑO
# =========================================================
datos_resumen_partido_especie <- datos %>%
  group_by(Partido_Monitoreo, Especies, anio) %>%
  summarise(
    prop = mean(result),      # proporcion de positivos
    Positivos = sum(result),  # cantidad de positivos
    n = n(),                  # total de muestras
    .groups = "drop"
  )


# =========================================================
# 6. GRAFICOS GENERALES
# =========================================================

# ---------------------------------------------------------
# 6.1 Barras: total muestreado y positivos por año
# ---------------------------------------------------------
ggplot(datos_resumen_partido_especie, aes(x = anio)) +
  geom_bar(aes(y = n, fill = "N muestreado"),
           stat = "identity", position = "dodge") +
  geom_bar(aes(y = Positivos, fill = "Positivos"),
           stat = "identity", position = "dodge") +
  labs(
    title = "",
    x = "Años",
    y = "Valor",
    fill = ""
  ) +
  scale_fill_manual(values = c("N muestreado" = "orange",
                               "Positivos" = "red"))


# ---------------------------------------------------------
# 6.2 Lineas: proporcion de positivos por especie y año
# ---------------------------------------------------------
ggplot(datos_resumen_partido_especie,
       aes(x = anio, y = prop, color = Especies, group = Especies)) +
  geom_line() +
  labs(
    title = "Proporción de casos positivos por especie anual",
    x = "Años",
    y = "Proporción de positivos",
    color = ""
  )


# ---------------------------------------------------------
# 6.3 Barras: proporcion de positivos por especie y año
# ---------------------------------------------------------
ggplot(datos_resumen_partido_especie,
       aes(x = anio, y = prop, fill = Especies)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Años",
    y = "Proporción de positivos",
    fill = ""
  )


# ---------------------------------------------------------
# 6.4 Barras: proporcion de positivos por partido y año
# ---------------------------------------------------------
ggplot(datos_resumen_partido_especie,
       aes(x = anio, y = prop, fill = Partido_Monitoreo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Años",
    y = "Proporción de positivos",
    fill = ""
  )


# ---------------------------------------------------------
# 6.5 Barras: cantidad de muestras por partido y año
# ---------------------------------------------------------
ggplot(datos_resumen_partido_especie,
       aes(x = anio, y = n, fill = Partido_Monitoreo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Años",
    y = "Cantidad de muestras",
    fill = ""
  )


# =========================================================
# 7. ANALISIS POR LOCALIDAD
# =========================================================
datos_resumen_localidad <- datos %>%
  group_by(Localidad_Monitoreo, Especies, anio) %>%
  summarise(
    prop = mean(result),
    Positivos = sum(result),
    n = n(),
    .groups = "drop"
  )


# ---------------------------------------------------------
# 7.1 Grafico de barras por localidad
# ---------------------------------------------------------
plot_localidades_all <- ggplot(datos_resumen_localidad,
                               aes(x = Localidad_Monitoreo)) +
  geom_bar(aes(y = n, fill = "N muestreado"),
           stat = "identity", position = "dodge") +
  geom_bar(aes(y = Positivos, fill = "Positivos"),
           stat = "identity", position = "dodge") +
  labs(
    title = "",
    x = "Localidad",
    y = "Valor",
    fill = ""
  ) +
  scale_fill_manual(values = c("N muestreado" = "orange",
                               "Positivos" = "red")) +
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )


# Guardar grafico
ggsave(
  filename = "C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/Graficos Estadisticos/Grafico_localidades_general.png",
  plot = plot_localidades_all,
  width = 30,
  height = 20,
  dpi = 300,
  units = "cm"
)

# Mostrar grafico
plot_localidades_all


# =========================================================
# 8. RESUMEN MAS DETALLADO: PARTIDO + LOCALIDAD + ESPECIE + AÑO
# =========================================================
datos_resumen_detalle <- datos %>%
  group_by(Partido_Monitoreo, Localidad_Monitoreo, Especies, anio) %>%
  summarise(
    prop = mean(result),
    n = n(),
    .groups = "drop"
  )


# ---------------------------------------------------------
# 8.1 Lineas: cantidad de muestras por partido, facet por especie
# ---------------------------------------------------------
ggplot(datos_resumen_detalle,
       aes(x = anio, y = n, color = Partido_Monitoreo, group = Partido_Monitoreo)) +
  geom_line() +
  facet_wrap(Especies ~ .) +
  labs(
    x = "Años",
    y = "Cantidad de muestras",
    color = ""
  )


# ---------------------------------------------------------
# 8.2 Lineas: cantidad de muestras por partido, facet por localidad
# ---------------------------------------------------------
ggplot(datos_resumen_detalle,
       aes(x = anio, y = n, color = Partido_Monitoreo, group = Partido_Monitoreo)) +
  geom_line() +
  facet_wrap(Localidad_Monitoreo ~ .) +
  labs(
    x = "Años",
    y = "Cantidad de muestras",
    color = ""
  )


# =========================================================
# 9. FILTRADO DE DATOS DEL AÑO 2022
# =========================================================

# Subconjuntos a partir de la base general
datos2022_positivos <- subset(datos, anio == 2022 & Resultado.Monitoreo == "Positivo")
datos2022_negativos <- subset(datos, anio == 2022 & Resultado.Monitoreo == "Negativo")

# Si se desea exportar los negativos:
# write.csv(datos2022_negativos, "datos2022_negativos.csv", row.names = FALSE)


# =========================================================
# 10. LECTURA DE BASE ESPECIFICA 2022
# =========================================================
datos2022 <- read.csv("datos2022.csv")

# Crear variable binaria para 2022
datos2022$result2022 <- as.numeric(datos2022$Resultado.Monitoreo == "Positivo")

# Verificar faltantes
sum(is.na(datos2022$Resultado.Monitoreo))  # no hay datos faltantes

# Separar fecha en dia, mes y anio
misFechas2022 <- do.call(rbind, strsplit(datos2022$Fecha.de.Muestra, "/"))
colnames(misFechas2022) <- c("dia", "mes", "anio")

# Convertir a numerico
misFechas2022 <- apply(misFechas2022, 2, as.numeric)

# Agregar al data frame
datos2022 <- cbind(datos2022, misFechas2022)


# =========================================================
# 11. RESUMEN MENSUAL 2022 POR PARTIDO
# =========================================================
datos_resumen2022 <- datos2022 %>%
  group_by(Partido_Monitoreo, anio, mes) %>%
  summarise(
    prop = mean(result2022),
    Positivos_2022 = sum(result2022),
    n = n(),
    .groups = "drop"
  )


# ---------------------------------------------------------
# 11.1 Barras: proporcion de positivos por partido y mes
# ---------------------------------------------------------
ggplot(datos_resumen2022,
       aes(x = factor(mes, labels = month.name),
           y = prop,
           fill = Partido_Monitoreo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Meses",
    y = "Proporción de positivos",
    fill = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )


# ---------------------------------------------------------
# 11.2 Barras: cantidad muestreada y positivos por mes
# ---------------------------------------------------------
ggplot(datos_resumen2022, aes(x = factor(mes, labels = month.name))) +
  geom_bar(aes(y = n, fill = "N muestreado"),
           stat = "identity", position = "dodge") +
  geom_bar(aes(y = Positivos_2022, fill = "Positivos"),
           stat = "identity", position = "dodge") +
  labs(
    title = "N muestreado y casos positivos de HLB para el año 2022",
    x = "Meses",
    y = "Valor",
    fill = ""
  ) +
  scale_fill_manual(values = c("N muestreado" = "orange",
                               "Positivos" = "red"))


# =========================================================
# 12. ANALISIS POR LOCALIDAD PARA 2022
# =========================================================
datos_resumen_localidad_2022 <- datos2022 %>%
  group_by(Localidad_Monitoreo, Especies, anio) %>%
  summarise(
    prop = mean(result2022),
    Positivos = sum(result2022),
    n = n(),
    .groups = "drop"
  )


# ---------------------------------------------------------
# 12.1 Grafico de barras por localidad para 2022
# ---------------------------------------------------------
plot_localidades_2022 <- ggplot(datos_resumen_localidad_2022,
                                aes(x = Localidad_Monitoreo)) +
  geom_bar(aes(y = n, fill = "N muestreado"),
           stat = "identity", position = "dodge") +
  geom_bar(aes(y = Positivos, fill = "Positivos"),
           stat = "identity", position = "dodge") +
  labs(
    title = "N muestreado y casos positivos de HLB por localidad - Año 2022",
    x = "Localidad",
    y = "Valor",
    fill = ""
  ) +
  scale_fill_manual(values = c("N muestreado" = "orange",
                               "Positivos" = "red")) +
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )


# Guardar grafico
ggsave(
  filename = "C:/Users/Usuario/Desktop/Hiperespectrales/ENTRE RIOS/Graficos Estadisticos/Grafico_localidades_2022.png",
  plot = plot_localidades_2022,
  width = 30,
  height = 20,
  dpi = 300,
  units = "cm"
)

# Mostrar grafico
plot_localidades_2022
