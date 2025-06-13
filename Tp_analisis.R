#Tp bioestadistica - Análisis estadistico

path="C:/Users/Equipo/Documents/Bioestadistica/TP_bioestadistica"
setwd(setwd(path))

datos_clean <- read.csv("C:/Users/Equipo/Documents/Bioestadistica/TP_bioestadistica/kidney_disease_EDA.csv", sep=",",header= TRUE)
print(head(datos_clean))

#Histograma bp vs ckd o notcdk

ggplot(datos_clean, aes(x = bp, fill = classification)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  scale_fill_manual(values = c("ckd" = "red", "notckd" = "purple")) +
  scale_x_continuous(breaks = seq(40, 180, 10)) + 
  labs(title = "Histograma de presión arterial (bp)",
       x = "Presión arterial",
       y = "Frecuencia",
       fill = "Clasificación") +
  theme_minimal()

#-------------------------------------------------
#-----------ANALISIS DE VARIABLES ----------------
#-------------------------------------------------

#A que edad se tiene mas predisposición a tener ckd
#Variable_1: Edad continua
#Variable_2: Clasificación (dicotomico)

#Analisis descriptivo: histograma

library(ggplot2)
library(dplyr)
library(ResourceSelection)
datos_filtrados <- datos_clean %>% filter(!is.na(age_group))

ggplot(datos_filtrados, aes(x = age_group, fill = classification)) +
  geom_bar(position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("ckd" = "red", "notckd" = "purple")) +
  labs(title = "Frecuencia por rango de edad y clasificación",
       x = "Rango de edad",
       y = "Cantidad de pacientes",
       fill = "Clasificación") +
  theme_light()

# Separar los datos por grupo
datos_ckd <- subset(datos_clean, classification == "ckd" & !is.na(age))
datos_nockd <- subset(datos_clean, classification == "notckd" & !is.na(age))

# Test de Shapiro-Wilk para cada grupo
shapiro.test(datos_ckd$age) #W = 0.9067, p-value = 3.912e-11
shapiro.test(datos_nockd$age) #W = 0.98622, p-value = 0.1454


#Tecnica: Regresión logística
#solo con edad
modelo_1 <- glm(classification == "ckd" ~ age, data = datos_clean,  family = poisson(link = "log"))
summary(modelo_1)

# Crear nueva data para predecir
nueva_edad <- data.frame(age = seq(min(datos_clean$age, na.rm=TRUE), max(datos_clean$age, na.rm=TRUE), length.out = 100))

pred <- predict(modelo_1, newdata = nueva_edad, type = "link", se.fit = TRUE)
nueva_edad$prob <- plogis(pred$fit)
nueva_edad$lower <- plogis(pred$fit - 1.96 * pred$se.fit)
nueva_edad$upper <- plogis(pred$fit + 1.96 * pred$se.fit)

# Graficar
ggplot(nueva_edad, aes(x = age, y = prob)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Probabilidad de CKD según edad",
       x = "Edad", y = "Probabilidad estimada de CKD")

hoslem.test(modelo_1$y, fitted(modelo_1), g = 10)

#agregando hipertension y diabetes
"""datos_clean$htn_bin <- ifelse(datos_clean$htn == "yes", 1, ifelse(datos_clean$htn == "no", 0, NA))
datos_clean$dm_bin <- ifelse(datos_clean$dm == "yes", 1, ifelse(datos_clean$dm == "no", 0, NA))
datos_modelo_full <- na.omit(datos_clean[, c("classification", "age", "htn_bin", "dm_bin")])

modelo_2 <- glm(I(classification == "ckd") ~ age + htn_bin + dm_bin, 
                family = "binomial", data = datos_modelo_full)
summary(modelo_2)
#nueva_edad <- data.frame(age = seq(min(datos_clean$age, na.rm=TRUE), max(datos_clean$age, na.rm=TRUE), length.out = 100))
nueva_edad <- data.frame(
  age = seq(min(datos_clean$age, na.rm = TRUE), max(datos_clean$age, na.rm = TRUE), length.out = 100),
  htn_bin = 1,
  dm_bin = 1
)
pred_2 <- predict(modelo_2, newdata = nueva_edad, type = "link", se.fit = TRUE)

nueva_edad$prob <- plogis(pred_2$fit)
nueva_edad$lower <- plogis(pred_2$fit - 1.96 * pred_2$se.fit)
nueva_edad$upper <- plogis(pred_2$fit + 1.96 * pred_2$se.fit)

# Graficar
ggplot(nueva_edad, aes(x = age, y = prob)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Probabilidad de CKD según edad",
       x = "Edad", y = "Probabilidad estimada de CKD")

hoslem.test(modelo_2$y, fitted(modelo_2), g = 10)"""
#


modelo_3 <- glm(I(classification == "ckd") ~ age + bp,
                data = datos_clean,
                family = poisson(link = "log"))
summary(modelo_3)
# Supongamos presión arterial promedio: 80
dato_pred <- data.frame(
  age = seq(min(datos_clean$age, na.rm = TRUE), max(datos_clean$age, na.rm = TRUE), length.out = 100), bp=seq(min(datos_clean$bp, na.rm = TRUE), max(datos_clean$bp, na.rm = TRUE), length.out = 100)  
)
pred_3 <- predict(modelo_3, newdata = dato_pred, type = "link", se.fit = TRUE)

nueva_edad$prob <- plogis(pred_3$fit)
nueva_edad$lower <- plogis(pred_3$fit - 1.96 * pred_3$se.fit)
nueva_edad$upper <- plogis(pred_3$fit + 1.96 * pred_3$se.fit)
ggplot(nueva_edad, aes(x = age, y = prob)) +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "lightgreen") +
  labs(title = "Probabilidad de CKD según edad (con presión arterial fija en 80)",
       x = "Edad",
       y = "Probabilidad estimada de CKD") +
  theme_minimal()

hoslem.test(modelo_3$y, fitted(modelo_3), g = 8)

