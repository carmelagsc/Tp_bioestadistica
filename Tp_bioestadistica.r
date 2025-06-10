path="C:/Users/Equipo/Documents/Bioestadistica/TP_bioestadistica"
setwd(setwd(path))

datos <- read.csv("kidney_disease.csv", sep=",",header= TRUE)
print(head(datos))