rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
library(dplyr)

#setwd("/Users/maiconfialho/Documents/Mestrado/2023-2/dmeyf2023/kaggle3") # Establezco el Directorio de Trabajo
setwd("C:/Users/maico/Documents/Mestrado/dmeyf2023/kaggle3/") # Establezco el Directorio de Trabajo
dataset <- fread("./experimentos_colaborativos/datos/competencia_03.csv.gz")
dataset <- dataset[order(numero_de_cliente, foto_mes)]



dataset <- dataset %>%
  mutate_all(funs(lag1 = lag(.), lag3 = lag(., 3), lag6 = lag(., 6)))

View(dataset)

fwrite(dataset,
        "./experimentos_colaborativos/datos/competencia_03_ec.csv.gz",
        logical01= TRUE,
        sep= "," )
