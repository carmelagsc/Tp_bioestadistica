rm(list=ls())

library(ggplot2)
library(dplyr)
library(tibble)

setwd("C:/Users/Mariana/Desktop/bio/bioestadistica")
datos <- read.csv("kidney_disease_EDA.csv")

cat_vars <- c("rbc","pc", "pcc", "ba", "htn", "dm", "cad", "appet", "pe", "ane", "classification")
datacategorica <- datos[, cat_vars]


cat_summary <- lapply(cat_vars, function(var) {
  valores <- datos[[var]]
  tabla <- table(valores, useNA = "ifany")
  tibble(
    variable = var,
    niveles = n_distinct(valores, na.rm = TRUE),
    categorias = paste(sort(unique(valores)), collapse = ", "),
    faltantes = sum(is.na(valores)),
    faltantes_pct = round(100 * mean(is.na(valores)), 2)
  )
})

cat_summary_df <- do.call(rbind, cat_summary)
print(cat_summary_df)

-----------------------------------------------
-----------------------------------------------
----------#Variables ordinales---------------

sort(unique(datos$al))
sort(unique(datos$su))
#copilot dice que sirve hacer esot antes de  kurkas wialliss 
datos$al <- factor(datos$al, levels = c(0, 1, 2, 3, 4, 5), ordered = TRUE)
datos$su <- factor(datos$su, levels = c(0, 1, 2, 3, 4, 5), ordered = TRUE)
dataordinales <- datos[,c ("su","al")]
