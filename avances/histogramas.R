rm(list=ls())

library(ggplot2)
library(dplyr)
library(tibble)

setwd("C:/Users/Mariana/Desktop/bio/bioestadistica")
datos <- read.csv("kidney_disease_EDA.csv")

# Vector con los nombres de las variables numéricas
vars <- c("bp", "age","sg", "bgr", "bu", "sc", "sod", "pot", "hemo", "pcv", "wc", "rc")

# Función para calcular binwidth óptimo usando Freedman–Diaconis
calc_binwidth <- function(x) {
  x <- x[!is.na(x)]
  iqr <- IQR(x)
  n <- length(x)
  if (iqr == 0 || n < 2) return(1)  # fallback si IQR = 0 o pocos datos
  bw <- 2 * iqr / (n^(1/3))
  return(bw)
}

# Crear histogramas uno por uno con binwidth óptimo
for (var in vars) {
  x <- datos[[var]]
  binwidth <- calc_binwidth(x)
  print(
    ggplot(datos, aes_string(x = var)) +
      geom_histogram(binwidth = binwidth, fill = "steelblue", color = "white") +
      labs(title = paste("Histograma de", var),
           x = var, y = "Frecuencia") +
      theme_minimal()
  )
}

