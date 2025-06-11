path="C:/Users/Equipo/Documents/Bioestadistica/TP_bioestadistica"
setwd(setwd(path))

datos <- read.csv("C:/Users/Equipo/Documents/Bioestadistica/TP_bioestadistica/kidney_disease.csv", sep=",",header= TRUE)
print(head(datos))

#Análisis exloratorio de datos
colnames(datos)
dim(datos)
sum(is.na(datos))
colSums(is.na(datos))
summary(datos)

ckd <- subset(datos, classification == "ckd")
notckd <- subset(datos, classification == "notckd")

print("Información sobre los datos clasificados con enfermedad renal")
dim(ckd)
sum(is.na(ckd))
colSums(is.na(ckd))
na_ckd <- rowSums(is.na(ckd))
na_ckd[na_ckd != 0]
summary(notckd)
summary(ckd)

print("Información sobre los datos clasificados sin enfermedad renal")
dim(notckd)
sum(is.na(notckd))
colSums(is.na(notckd))
na_not <- rowSums(is.na(notckd))
na_not[na_not != 0]
class(na_not)
summary(notckd)
#Hay que ver que hacer con los NA, podemos ver de poner un numero representativo o dejarlo asi. Si un mismo paciente tiene muchos NA yo lo sacaría.

#histograma de edades
library(ggplot2)
# Asegurarse de que age es numérico
datos$age <- as.numeric(datos$age)

# Crear variable nueva con rangos de edad
datos$age_group <- cut(datos$age,
                    breaks = seq(0, 100, by = 10),  # de 0 a 100 en saltos de 10
                    right = FALSE,                  # intervalo izquierdo cerrado
                    labels = paste(seq(1, 91, 10), seq(10, 100, 10), sep = "-"))


x11() ; ggplot(datos, aes(x = age_group, fill = classification)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("ckd" = "pink", "notckd" = "purple")) +
  labs(title = "Frecuencia por rango de edad y clasificación",
       x = "Rango de edad",
       y = "Cantidad de pacientes",
       fill = "Clasificación") +
  theme_light()