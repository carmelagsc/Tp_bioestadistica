library(MASS)     # Para boxcox
library(dplyr)    # Para manipulación de datos
library(tibble)   # Para crear tabla
library(ggplot2)  # Para posibles gráficos


# Variables numéricas a transformar
vars <- c("bp", "age","sg", "bgr", "bu", "sc", 
          "sod", "pot", "hemo", "pcv", "wc", "rc")

# Inicializar lista para guardar resultados
resultados <- list()

for (var in vars) {
  x <- notckd_num[[var]]
  x <- na.omit(x)
  x <- x[x > 0]  # Box-Cox requiere valores > 0
  
  if (length(x) > 10) {  # Asegura tamaño mínimo
    modelo <- lm(x ~ 1)
    bc <- boxcox(modelo, lambda = seq(-2, 2, 0.1), plotit = TRUE)
    
    lambda_opt <- bc$x[which.max(bc$y)]
    
    # Aplicar transformación
    if (abs(lambda_opt) < 1e-3) {
      x_trans <- log(x)
    } else {
      x_trans <- (x^lambda_opt - 1) / lambda_opt
    }
    
    # Test de normalidad
    shapiro_res <- shapiro.test(x_trans)
    
    # Guardar resultados
    resultados[[var]] <- tibble(
      Variable = var,
      Lambda = round(lambda_opt, 3),
      Shapiro_p = round(shapiro_res$p.value, 4),
      Normal = ifelse(shapiro_res$p.value > 0.05, "✅ Sí", "❌ No")
    )
  }
}

# Combinar todos los resultados en una tabla
tabla_final <- bind_rows(resultados)
print(tabla_final)

