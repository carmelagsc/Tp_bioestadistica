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
      geom_histogram(binwidth = binwidth, fill = "orange", color = "white") +
      labs(title = paste("Histograma de", log_var),
           x = log_var, y = "Frecuencia") +
      theme_minimal()
  )
}

resultados_shapiro <- lapply(vars, function(var) {
  x <- datos[[log_var]]
  x <- x[!is.na(x)]  # eliminar NAs
  if (length(x) >= 3 && length(x) <= 5000) {
    test <- shapiro.test(x)
    return(data.frame(
      Variable = var,
      W = round(test$statistic, 4),
      p_value = round(test$p.value, 4),
      Normal = ifelse(test$p.value > 0.05, "Sí", "No")
    ))
  } else {
    return(data.frame(
      Variable = var,
      W = NA,
      p_value = NA,
      Normal = "Insuficiente datos"
    ))
  }
})

# Combinar resultados en un solo data frame
tabla_shapiro <- do.call(rbind, resultados_shapiro)

# Mostrar la tabla
print(tabla_shapiro)