path="C:/Users/Equipo/Documents/Bioestadistica/TP_bioestadistica"
setwd(setwd(path))

datos_clean <- read.csv("C:/Users/Equipo/Documents/Bioestadistica/TP_bioestadistica/kidney_disease_EDA.csv", sep=",",header= TRUE)
datos_clean <- subset(datos_clean, al != 5)
print(head(datos_clean))

datos_ckd <- subset(datos_clean, classification == "ckd" & !is.na(age))
datos_nockd <- subset(datos_clean, classification == "notckd" & !is.na(age))
#¿la presion arterial promedio difiere significativamente entre pacientes con distintas conscentrciones de  albumina?

# Paquetes necesarios
if (!require(car)) install.packages("car")
library(car)
library(ggplot2)

# 1. Preparar datos
df_anova <- na.omit(datos_clean[, c("bp", "al")])
df_anova$al <- as.factor(df_anova$al)

# 2. Test de normalidad (Shapiro-Wilk por grupo)
cat("📌 Test de normalidad (Shapiro por grupo):\n")
normalidad <- by(df_anova$bp, df_anova$al, shapiro.test)
print(normalidad)

# 3. Test de homogeneidad de varianzas (Levene)
cat("\n📌 Test de homogeneidad de varianzas (Levene):\n")
test_levene <- leveneTest(bp ~ al, data = df_anova)
print(test_levene)

# 4. Decidir: ANOVA o Kruskal
p_levene <- test_levene$`Pr(>F)`[1]
p_shapiro <- sapply(normalidad, function(x) x$p.value)
cumple_normalidad <- all(p_shapiro > 0.05)
cumple_varianzas <- p_levene > 0.05

if (cumple_normalidad & cumple_varianzas) {
  cat("\n✅ Se cumplen los supuestos. Se realiza ANOVA:\n")
  modelo_anova <- aov(bp ~ al, data = df_anova)
  summary(modelo_anova)
  cat("\n📌 Post-hoc Tukey:\n")
  print(TukeyHSD(modelo_anova))
} else {
  cat("\n⚠️ No se cumplen los supuestos. Se realiza prueba de Kruskal-Wallis:\n")
  print(kruskal.test(bp ~ al, data = df_anova))
}

# 5. Gráfico de boxplot
ggplot(df_anova, aes(x = al, y = bp)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Presión arterial según nivel de albúmina",
       x = "Nivel de albúmina (al)", y = "Presión arterial (bp)") +
  theme_minimal()


# Cargar los datos
datos_clean <- read.csv("C:/Users/Equipo/Documents/Bioestadistica/TP_bioestadistica/kidney_disease_EDA.csv", sep=",", header=TRUE)

# Filtrar el grupo 5 de albumina
datos_clean <- subset(datos_clean, al != 5)

# Cargar paquetes necesarios
library(car)

# 1. Test de normalidad (Shapiro-Wilk)
shapiro_test <- shapiro.test(datos_clean$bp)

# 2. Histograma y QQ Plot para visualizar normalidad
hist(datos_clean$bp, main="Histograma de presión sanguínea", xlab="Presión sanguínea")
qqnorm(datos_clean$bp)
qqline(datos_clean$bp)

# 3. Test de igualdad de varianzas (Levene y Bartlett)
levene_test <- leveneTest(bp ~ as.factor(al), data = datos_clean)
bartlett_test <- bartlett.test(bp ~ as.factor(al), data = datos_clean)

# Mostrar resultados
print("Resultados del test de normalidad (Shapiro-Wilk):")
print(shapiro_test)

print("Resultados del test de igualdad de varianzas:")
print("Levene Test:")
print(levene_test)
print("Bartlett Test:")
print(bartlett_test)


library(dunn.test)

# Prueba de Kruskal-Wallis
kruskal_test <- kruskal.test(bp ~ as.factor(al), data = datos_clean)

# Mostrar resultados
print("Resultados de la prueba de Kruskal-Wallis:")
print(kruskal_test)

# Prueba de Dunn con corrección de Bonferroni
dunn_test <- dunn.test(datos_clean$bp, datos_clean$al, method="bonferroni")

# Mostrar resultados
print("Resultados de la prueba de Dunn con corrección de Bonferroni:")
print(dunn_test)