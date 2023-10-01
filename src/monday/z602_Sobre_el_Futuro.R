##
## Sobre El Futuro
##
## ---------------------------
## Step 1: Setup
## ---------------------------
##
## The future is not something to predict. The future is something to build.
## --- Franco Ongaro

# Profundizaremos en los puntos de corte.
# IMPORTANTE: En esta competencia se puede entrenar usando mayo. Sin embargo,
# vamos aprovechar (y recomendar que usted también lo haga) marzo para
# experimentar contra mayo.

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")

# Poner la carpeta de la materia de SU computadora local
setwd("/home/aleb/dmeyf23/datasets")
# Poner sus semillas
semillas <- c(17, 19, 23, 29, 31)

# Cargamos los datasets y nos quedamos solo con 202103 y 202105
dataset <- fread("competencia_02.csv.gz")
marzo <- dataset[foto_mes == 202103]
mayo <- dataset[foto_mes == 202105]

rm(dataset)

clase_binaria <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
marzo$clase_ternaria <- NULL

## ---------------------------
## Step 2: Un modelo simple de LGBM
## ---------------------------

# Armamos el dataset de train para LGBM
dtrain  <- lgb.Dataset(data = data.matrix(marzo), label = clase_binaria)

model_lgm <- lightgbm(data = dtrain,
            nrounds = 100,
            params = list(objective = "binary",
                          max_bin = 15,
                          min_data_in_leaf = 4000,
                          learning_rate = 0.05),
             verbose = -1)

## ---------------------------
## Step 3: Veamos como funcionó en mayo
## ---------------------------

mayo$pred <- predict(model_lgm, data.matrix(mayo[, 1:154]))
sum((mayo$pred > 0.025) * ifelse(mayo$clase_ternaria == "BAJA+2", 273000, -7000))

## ---------------------------
## Step 4: Veamos cuán distintos los scores entregados
## ---------------------------

length(mayo$pred)
length(unique(mayo$pred))

## Preguntas
## - ¿Qué diferencia observa con respecto a ?

## ---------------------------
## Step 4: En el leaderboard público.
## ---------------------------

# Simulamos un Leaderboard público:
set.seed(semillas[1])
split <- caret::createDataPartition(mayo$clase_ternaria, p = 0.70, list = FALSE)

# Vemos la cantidad de casos que estaríamos mandando:clase_ternaria
sum(mayo$pred > 0.025) # En mi caso dice que estaría mandando 7744

# Y obtendríamos una ganancia de
# Privado
sum((mayo$pred[split] > 0.025) * ifelse(mayo$clase_ternaria[split] == "BAJA+2", 273000, -7000)) / 0.7

# Público
sum((mayo$pred[-split] > 0.025) * ifelse(mayo$clase_ternaria[-split] == "BAJA+2", 273000, -7000)) / 0.3

# Pero... que pasa si mandamos otra cantidad de casos?
# Vamos a mandar los N mejores casos, de a separaciones de M

## ---------------------------
## Step 4: Buscando el mejor punto de corte en el leaderboard público.
## ---------------------------

# Ordenamos el dataset segun su probabilidad de forma ascendente
setorder(mayo, cols = -pred)

# PROBAR MULTIPLES VALORES
set.seed(semillas[3])
m <- 500
f <- 5000
t <- 15000

leaderboad <- data.table()
split <- caret::createDataPartition(mayo$clase_ternaria, p = 0.70, list = FALSE)
mayo$board[split] <- "privado"
mayo$board[-split] <- "publico"
for (s in seq(f, t, m)) {
    privado <- mayo[1:s, sum(ifelse(board == "privado",
        ifelse(clase_ternaria == "BAJA+2", 273000, -7000), 0)) / 0.5]
    publico <- mayo[1:s, sum(ifelse(board == "publico",
        ifelse(clase_ternaria == "BAJA+2", 273000, -7000), 0)) / 0.5]
    leaderboad <- rbindlist(list(leaderboad,
                        data.table(envio = s, board = "privado", valor = privado),
                        data.table(envio = s, board = "publico", valor = publico)
                        ))
}
# Graficamos
ggplot(leaderboad[board == "publico"], aes(x = envio, y = valor, color = board)) + geom_line()

ggplot(leaderboad, aes(x = envio, y = valor, color = board)) + geom_line()

