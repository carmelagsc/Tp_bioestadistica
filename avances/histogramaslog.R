rm(list=ls())

library(ggplot2)
library(dplyr)
library(tibble)

setwd("C:/Users/Mariana/Desktop/bio/bioestadistica")
datos <- read.csv("kidney_disease_EDA.csv")
# Vector de variables numéricas originales
vars <- c("bp", "age","sg", "bgr", "bu", "sc", 
          "sod", "pot", "hemo", "pcv", "wc", "rc")

# Crear versiones logarítmicas y agregarlas al data frame
for (var in vars) {
  log_var <- paste0("log_", var)
  datos[[log_var]] <- log1p(datos[[var]])  # log(x + 1)
}

# Graficar histogramas para cada variable log-transformada
for (var in vars) {
  log_var <- paste0("log_", var)
  x <- datos[[log_var]]
  
  # Calcular binwidth óptimo usando Freedman–Diaconis
  x <- x[is.finite(x)]
  binwidth <- if (length(x) < 2 || IQR(x) == 0) 1 else 2 * IQR(x) / (length(x)^(1/3))
  
  print(
    ggplot(data.frame(x), aes(x = x)) +
      geom_histogram(binwidth = binwidth, fill = "darkorange", color = "white") +
      labs(title = paste("Histograma de", log_var),
           x = log_var, y = "Frecuencia") +
      theme_minimal()
  )
}