# Limpiar entorno
rm(list = ls())


# Cargar librerías necesarias
library(openxlsx)
library(ggplot2)
library(tidyr)
library(car)
library(dplyr)
library(FSA) # Para pruebas post-hoc

# Establecer directorio de trabajo
setwd("C:/Users/Mariana/Desktop/bio/bioestadistica")

# Cargar datos
datos <- read.csv("kidney_disease_EDA.csv")

# Paso 1: Ver cuántos valores válidos de BP hay por categoría de SU
resumen_su <- datos %>%
  group_by(su) %>%
  summarise(
    total = n(),
    bp_no_na = sum(!is.na(bp))
  )
print(resumen_su)

# Paso 2: Filtrar filas con BP no NA
datos_filtrados <- datos %>% filter(!is.na(bp), !is.na(su))

# Paso 3: Verificar normalidad (Shapiro-Wilk por grupo de SU)
shapiro_results <- datos_filtrados %>%
  group_by(su) %>%
  summarise(p_value = shapiro.test(bp)$p.value)
print(shapiro_results)

# Paso 4: Verificar homogeneidad de varianzas (Levene)
levene_result <- leveneTest(bp ~ as.factor(su), data = datos_filtrados)
print(levene_result)

# Paso 5: Kruskal-Wallis (Alternativa no paramétrica)
kruskal_result <- kruskal.test(bp ~ as.factor(su), data = datos_filtrados)
print(kruskal_result)

# Paso 6: Prueba post-hoc (Dunn)
dunn_result <- dunnTest(bp ~ as.factor(su), data = datos_filtrados, method = "bonferroni")
print(dunn_result)
