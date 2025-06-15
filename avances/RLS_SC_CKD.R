# Limpiar el entorno de trabajo
rm(list = ls())

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tibble)
library(FSA)
library(ResourceSelection)


# Leer los datos
setwd("C:/Users/Mariana/Desktop/bio/bioestadistica")
datos <- read.csv("kidney_disease_EDA.csv")

# OBJETIVO:
# Evaluar si la creatinina sérica logarítmica (log_sc) permite predecir
# la presencia de enfermedad renal crónica (classification)

# 1. Preparar la variable dependiente (respuesta)
datos$classification <- factor(datos$classification, levels = c("notckd", "ckd"))
# La variable 'ckd' se toma como la categoría positiva (evento de interés)

# 2. Transformar la creatinina con logaritmo natural (+1 para evitar log(0))
datos$log_sc <- log1p(datos$sc)

# 3. Ajustar modelo logístico simple con log_sc
modelo_A <- glm(classification ~ log_sc, data = datos, family = "binomial")

# 4. Ver resumen del modelo (coeficientes, significancia estadística)
summary(modelo_A)

# 5. Calcular Odds Ratios e intervalos de confianza del 95%
exp(cbind(OddsRatio = coef(modelo_A), confint(modelo_A)))

# 6. Graficar la relación entre log_sc y la probabilidad estimada de CKD
ggplot(datos, aes(x = log_sc, y = as.numeric(classification) - 1)) +
  geom_jitter(height = 0.05, alpha = 0.3, color = "steelblue") +
  stat_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "firebrick", linewidth = 1.2) +
  labs(
    title = "Probabilidad de CKD según log(Creatinina sérica)",
    x = "log(Creatinina sérica)",
    y = "Probabilidad estimada de CKD",
    caption = "Cada punto azul representa un paciente individual (jitter aplicado para evitar superposición)"
  ) +
  theme_minimal()

# 7. Evaluar observaciones influyentes (outliers, leverage)
influence.measures(modelo_A)
# También podés revisar visualmente con:
plot(modelo_A)

# 8. Evaluar desempeño del modelo: predicción y matriz de confusión
df_modelo <- model.frame(modelo_A)  # Casos completos que usó el modelo
prob <- predict(modelo_A, type = "response")
clasif_pred <- ifelse(prob >= 0.5, "ckd", "notckd")
clasif_pred <- factor(clasif_pred, levels = c("notckd", "ckd"))

# Generar matriz de confusión
table(Predicho = clasif_pred, Real = df_modelo$classification)


# Aplicar la prueba al modelo
# Debe usarse sobre datos sin valores NA y con binaria numérica (0/1)
# Por eso extraemos la base usada por el modelo
df_modelo <- model.frame(modelo_A)

# Variable dependiente como numérica (0 = notckd, 1 = ckd)
y <- as.numeric(df_modelo$classification) - 1
p <- predict(modelo_A, type = "response")

# Prueba de Hosmer–Lemeshow con 10 grupos
hoslem.test(y, p, g = 10)