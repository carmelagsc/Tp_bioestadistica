rm(list=ls())

library(ggplot2)
library(dplyr)

setwd("C:/Users/Mariana/Desktop/bio/bioestadistica")
datos <- read.csv("kidney_disease_EDA.csv")

summary(datos[, c("bp", "age","sg", "bgr", "bu", "sc", "sod", "pot", "hemo", "pcv", "wc", "rc")])

summary(datos[, c("bp", "age","sg", "bgr", "bu", "sc", "sod", "pot", "hemo", "pcv", "wc", "rc")])
unique(datos$pcv)
unique(datos$wc)
unique(datos$rc)

# Función para limpiar y convertir
limpiar_columna_numerica <- function(columna) {
  columna <- gsub("\\t", "", columna)   # elimina tabulaciones
  columna <- gsub("\\?", NA, columna)   # reemplaza "?" por NA
  columna <- as.numeric(columna)        # convierte a numérico
  return(columna)
}

# Aplicarlo a tus columnas
datos$pcv <- limpiar_columna_numerica(datos$pcv)
datos$wc  <- limpiar_columna_numerica(datos$wc)
datos$rc  <- limpiar_columna_numerica(datos$rc)

# Guardar solo esas modificaciones en el CSV original
#write.csv(datos, "kidney_disease_EDA.csv", row.names = FALSE)

summary(datos[, c("bp", "age","sg", "bgr", "bu", "sc", "sod", "pot", "hemo", "pcv", "wc", "rc")])


# Cargar dplyr por si no está cargado
library(dplyr)

# Lista de variables numéricas
vars <- c("bp", "age","sg", "bgr", "bu", "sc", "sod", "pot", "hemo", "pcv", "wc", "rc")

# Convertir a numéricas por si alguna es texto
datos_num <- datos[, vars] %>% mutate(across(everything(), as.numeric))

# Detectar outliers con método IQR
outliers_por_variable <- lapply(vars, function(var) {
  x <- datos_num[[var]]
  x <- x[!is.na(x)]
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  outliers <- sum(x < lower | x > upper)
  total <- length(x)
  porcentaje <- round(100 * outliers / total, 2)
  data.frame(variable = var, outliers = outliers, total = total, porcentaje = porcentaje)
})

# Unir resultados
outliers_df <- do.call(rbind, outliers_por_variable)
print(outliers_df)

# Creamos una lista vacía para guardar los valores outliers por variable
valores_outliers_por_variable <- list()

# Recorremos cada variable numérica
for (var in vars) {
  x <- datos_num[[var]]
  x_no_na <- x[!is.na(x)]
  
  # Cálculo del IQR
  Q1 <- quantile(x_no_na, 0.25)
  Q3 <- quantile(x_no_na, 0.75)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  # Filtrar outliers
  outlier_vals <- x[x < lower | x > upper]
  
  # Guardar en la lista
  valores_outliers_por_variable[[var]] <- outlier_vals[!is.na(outlier_vals)]
}

# Convertir la lista a un data frame "rellenado" (tipo columna por variable)
max_len <- max(sapply(valores_outliers_por_variable, length))
outliers_wide <- as.data.frame(
  lapply(valores_outliers_por_variable, function(x) {
    length(x) <- max_len  # rellena con NA
    return(x)
  })
)

# Ver el resultado
print(outliers_wide)

