check_glm_binomial <- function(formula, data) {
  mf <- model.frame(formula, data = data, na.action = na.pass)
  response <- mf[[1]]
  predictors <- mf[-1]
  
  cat("Chequeo para GLM con familia binomial (regresión logística)\n\n")
  
  # 1. Variable respuesta binaria
  if (is.factor(response)) {
    if (length(levels(response)) != 2) {
      cat("❌ Variable respuesta factor NO tiene 2 niveles.\n")
      return(FALSE)
    }
  } else {
    unique_vals <- unique(na.omit(response))
    if (!all(unique_vals %in% c(0,1))) {
      cat("❌ Variable respuesta NO es binaria (0/1).\n")
      return(FALSE)
    }
  }
  cat("✔ Variable respuesta binaria correcta.\n")
  
  # 2. Valores faltantes
  na_counts <- sapply(mf, function(x) sum(is.na(x)))
  if (any(na_counts > 0)) {
    cat("⚠️ Hay valores faltantes en variables:\n")
    print(na_counts[na_counts > 0])
  } else {
    cat("✔ No hay valores faltantes.\n")
  }
  
  # 3. Multicolinealidad: correlación entre variables numéricas
  if (length(predictors) > 1) {
    num_preds <- sapply(predictors, is.numeric)
    if (sum(num_preds) > 1) {
      cor_mat <- cor(mf[, names(predictors)[num_preds]], use = "complete.obs")
      cat("\n✔ Matriz de correlación entre predictores numéricos:\n")
      print(round(cor_mat, 2))
      
      high_cor <- which(abs(cor_mat) > 0.8 & abs(cor_mat) < 1, arr.ind = TRUE)
      if (nrow(high_cor) > 0) {
        cat("\n⚠️ Atención: correlaciones altas (>0.8) entre predictores:\n")
        for (i in seq_len(nrow(high_cor))) {
          r1 <- rownames(cor_mat)[high_cor[i,1]]
          r2 <- colnames(cor_mat)[high_cor[i,2]]
          cat(sprintf("  - %s y %s: %.2f\n", r1, r2, cor_mat[high_cor[i,1], high_cor[i,2]]))
        }
      }
    } else {
      cat("✔ No hay suficientes variables numéricas para chequear correlación.\n")
    }
  } else {
    cat("✔ Solo una variable predictora, no se chequea correlación.\n")
  }
  
  cat("\nResultado: las variables parecen válidas para un GLM binomial.\n")
  return(TRUE)
}

check_glm_binomial(I(classification == "ckd") ~ age + bp, datos_clean)

datos_filtrados <- na.omit(datos_clean[, c("classification", "age", "bp")])

modelo <- glm(I(classification == "ckd") ~ age + bp,
              data = datos_filtrados,
              family = binomial())
summary(modelo)
# 0. Ajustar el modelo (por si aún no lo hiciste)
modelo <- glm(I(classification == "ckd") ~ age + bp, 
              family = binomial(), data = datos_filtrados)

# 1. Graficar probabilidades predichas (en función de age y bp)
library(ggplot2)

datos_filtrados$prob_ckd <- predict(modelo, type = "response")

ggplot(datos_filtrados, aes(x = age, y = prob_ckd, color = factor(bp))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Probabilidad predicha de CKD según edad y presión arterial",
       x = "Edad", y = "Probabilidad de CKD", color = "Presión arterial") +
  theme_minimal()

# 2. Evaluar performance del modelo
# Matriz de confusión y accuracy
pred_class <- ifelse(datos_filtrados$prob_ckd > 0.5, 1, 0)
tabla_conf <- table(Predicho = pred_class, Real = datos_filtrados$classification == "ckd")
print(tabla_conf)

accuracy <- sum(diag(tabla_conf)) / sum(tabla_conf)
cat("\nAccuracy del modelo:", round(accuracy, 3), "\n")

# 3. Test de Hosmer-Lemeshow
# Instalar si no tenés el paquete
if (!require(ResourceSelection)) install.packages("ResourceSelection")
library(ResourceSelection)

hoslem <- hoslem.test(as.numeric(datos_filtrados$classification == "ckd"), 
                      datos_filtrados$prob_ckd, g = 8)
print(hoslem)

