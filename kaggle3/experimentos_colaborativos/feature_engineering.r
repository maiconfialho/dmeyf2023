rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
library(dplyr)

setwd("/Users/maiconfialho/Documents/Mestrado/2023-2/dmeyf2023/kaggle3") # Establezco el Directorio de Trabajo
#setwd("C:/Users/maico/Documents/Mestrado/dmeyf2023/") # Establezco el Directorio de Trabajo
dataset <- fread("./datos/competencia_03.csv.gz")
dataset <- dataset[order(numero_de_cliente, foto_mes)]

setDT(dataset)

lag_cols <- c("lag1", "lag3", "lag6")  # Names for lag columns

numeric_cols <- sapply(dataset, is.numeric)  # Find numeric columns

for (lag in lag_cols) {
  for (col in names(dataset)[numeric_cols]) {
    new_col_name <- paste0(col, "_", lag)
    dataset[, (new_col_name) := shift(get(col), n = as.numeric(substr(lag, 4, 4))), with = FALSE]
  }
}

head(dataset)

fwrite(dataset,
        "./datos/competencia_03_ec.csv.gz",
        logical01= TRUE,
        sep= "," )
