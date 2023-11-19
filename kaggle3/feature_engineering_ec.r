rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
library(dplyr)

#setwd("/Users/maiconfialho/Documents/Mestrado/2023-2/dmeyf2023/kaggle3") # Establezco el Directorio de Trabajo
setwd("C:/Users/maico/Documents/Mestrado/dmeyf2023/kaggle3/") # Establezco el Directorio de Trabajo
dataset_si <- fread("./datos/dataset_sem_inputar.csv.gz")
dataset_media <- fread("./datos/dataset_inputado_media.csv.gz")
dataset_mediana <- fread("./datos/dataset_inputado_mediana.csv.gz")

dataset_si <- dataset_si[order(numero_de_cliente, foto_mes)]
dataset_media <- dataset_media[order(numero_de_cliente, foto_mes)]
dataset_mediana <- dataset_mediana[order(numero_de_cliente, foto_mes)]



dataset_si <- dataset_si %>%
  mutate_all(funs(lag1 = lag(.), lag3 = lag(., 3), lag6 = lag(., 6)))

dataset_media <- dataset_media %>%
  mutate_all(funs(lag1 = lag(.), lag3 = lag(., 3), lag6 = lag(., 6)))

dataset_mediana <- dataset_mediana %>%
  mutate_all(funs(lag1 = lag(.), lag3 = lag(., 3), lag6 = lag(., 6)))

#View(dataset_si)

fwrite(dataset_si,
        "./datos/dataset_sem_inputar_ec.csv.gz",
        logical01= TRUE,
        sep= "," )

fwrite(dataset_media,
        "./datos/dataset_inputado_media_ec.csv.gz",
        logical01= TRUE,
        sep= "," )

fwrite(dataset_mediana,
        "./datos/dataset_inputado_mediana_ec.csv.gz",
        logical01= TRUE,
        sep= "," )