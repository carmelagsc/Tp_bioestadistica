rm(list=ls())

library(ggplot2)
library(dplyr)
library(tibble)
library(FSA)

setwd("C:/Users/Mariana/Desktop/bio/bioestadistica")
datos <- read.csv("kidney_disease_EDA.csv")

#Fusionar niveles superiores
datos$al_mod <- as.character(datos$al)
datos$al_mod[datos$al_mod == "5"] <- "4"
datos$al_mod <- factor(datos$al_mod, levels = c("0", "1", "2", "3", "4"), ordered = TRUE)

# Transformación logarítmica de bp
datos$log_bp <- log1p(datos$bp)

# Filtrar NA para ambas variables
bp_sin_na     <- na.omit(datos$bp)
log_bp_sin_na <- na.omit(datos$log_bp)

# Test de normalidad de Shapiro–Wilk
shapiro_bp     <- shapiro.test(bp_sin_na)
shapiro_log_bp <- shapiro.test(log_bp_sin_na)

# Mostrar resultados
cat("Shapiro–Wilk para bp:\n")
print(shapiro_bp)
cat("\nShapiro–Wilk para log(bp):\n")
print(shapiro_log_bp)

----------------------------------------------
----------------------------------------------
  #no para metrico 

# Test sobre bp original
kruskal_bp <- kruskal.test(bp ~ al_mod, data = datos)
cat("Kruskal–Wallis sobre bp:\n")
print(kruskal_bp)

# Test sobre log(bp)
kruskal_log_bp <- kruskal.test(log_bp ~ al_mod, data = datos)
cat("\nKruskal–Wallis sobre log(bp):\n")
print(kruskal_log_bp)

##Existe evidencia estadísticamente significativa para afirmar que la presión arterial (bp)
##difiere entre al menos dos niveles de albúmina en orina (al_mod). 
##Esto refuerza la hipótesis de que el nivel de proteinuria se asocia con cambios en la presión

--------------------------------------------
-------------------------------------------
datos$al_mod2 <- factor(datos$al_mod, ordered = FALSE)

  
# Test post-hoc de Dunn para bp
dunn_bp <- dunnTest(bp ~ al_mod2, data = datos, method = "bonferroni")
print(dunn_bp)

# Test post-hoc de Dunn para log_bp
dunn_log_bp <- dunnTest(log_bp ~ al_mod2, data = datos, method = "bonferroni")
print(dunn_log_bp)



#El análisis post-hoc de Dunn con corrección de Bonferroni 
#identificó una diferencia significativa únicamente entre 
#los niveles 0 y 2 de albúmina, lo cual sugiere que niveles 
#moderados de proteinuria se asocian con una presión arterial
#más elevada que en pacientes sin presencia de albúmina
#en orina. > > No se hallaron diferencias adicionales 
#significativas entre otros niveles. 
#El patrón se mantuvo consistente tanto en escala original 
#como logarítmica.

------------------------------------------
------------------------------------------
# Asegurate de que 'al' sea factor
datos$al <- factor(datos$al, levels = c(0,1,2,3,4,5), ordered = TRUE)

ggplot(datos, aes(x = al)) +
  geom_bar(fill = "royalblue") +
  labs(
    title = "Distribución general de niveles de albúmina (al)",
    x = "Nivel de albúmina en orina",
    y = "Frecuencia"
  ) +
  theme_minimal()


