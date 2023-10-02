##
## Sobre Campos
##
## ---------------------------
## Step 1: Cargando los datos y las librerías
## ---------------------------
##
## Genius is one percent inspiration and 99 percent perspiration
## --- ~~Thomas Edison~~ Kate Sanborn

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")


# Poner la carpeta de la materia de SU computadora local
setwd("/home/aleb/dmeyf23")
# Poner sus semillas
semillas <- c(17, 19, 23, 29, 31)

# Cargamos el dataset
dataset <- fread("./datasets/competencia_01.csv")

# Nos quedamos solo con el 202103
dataset <- dataset[foto_mes == 202103]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

set.seed(semillas[1])

calcular_ganancia <- function(modelo, test) {
    pred_testing <- predict(modelo, test, type = "prob")
    sum(
        (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                          273000, -7000) / 0.3
    )
}

## ---------------------------
## Step 2: Importancia de variables
## ---------------------------

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dataset$clase_binaria,
                     p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]

# Antes de empezar vamos a ver la importancia de variables
modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 1000,
                minbucket = 40,
                maxdepth = 7)

calcular_ganancia(modelo, dtest)
print(modelo$variable.importance)


## Preguntas
## - ¿Cuáles son las variables más importantes para el modelo?
## - ¿Cómo calcula RPART la importancia de una variable?
## - ¿Es la única forma de calcular la importancia de una variable?

## ---------------------------
## Step 3: Datos nulos
## ---------------------------

# Tenemos variables con nulos en el dataset?
summary(dtrain)

# En el summary del modelo buscamos un corte donde la primera variable
# tenga missing.
summary(modelo)

## Preguntas
## - ¿Cómo operó con la variable nula?
## - ¿Hace falta imputar las variables para que el árbol abra?

# Actividad en clase:
# -------------------

# Impute una variable que sea importante y vea si mejora tanto su ganancia
# y su importancia. Un ejemplo

experimento <- function(ds, semillas, cp = -1, minsplit = 1000, minbucket = 200, maxdepth = 7) {
    gan <- c()
    for (s in semillas) {
        set.seed(s)
        in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
            list = FALSE)
        train  <-  dataset[in_training, ]
        test   <-  dataset[-in_training, ]

        r <- rpart(clase_binaria ~ .,
                    data = train,
                    xval = 0,
                    cp = cp,
                    minsplit = minsplit,
                    minbucket = minbucket,
                    maxdepth = maxdepth)

        gan <- c(gan, calcular_ganancia(r, test))
    }
    print(mean(gan))
    r <- rpart(clase_binaria ~ .,
            data = ds,
            xval = 0,
            cp = cp,
            minsplit = minsplit,
            minbucket = minbucket,
            maxdepth = maxdepth)
    print(r$variable.importance)
}
experimento(dataset, semillas)

# Imputamos los nulos de una variable con ceros
print(sum(is.na(dataset$Visa_mlimitecompra)))
dataset[, Visa_mlimitecompra_2 := ifelse(is.na(Visa_mlimitecompra), 0, Visa_fechaalta)]
experimento(dataset, semillas)

## Comparta sus resultados

## ---------------------------
## Step 4: Correlaciones
## ---------------------------

# Varios modelos en los que entren dos variables muy correlacionadas se 
# romperían. Sucede esto con los árboles?

# Actividad en clase:
# -------------------
# Cree una variable muy correlacionada con su mejor variables y comparta que
# sucede.

## ---------------------------
## Step 5: Outliers
## ---------------------------

# Veamos el boxplot de una variable muy importante según nuestro árbol
ggplot(dataset, aes(x=ctrx_quarter)) + geom_boxplot()

# Vemos la distribución de los deciles
quantile(dataset$ctrx_quarter, probs = c(0,0.5, 0.75, 0.9, 0.95, 0.99, 1))

## Preguntas
## - ¿Qué tan frecuentes considera estas dispersiones en los datasets?

# Reduzcamos la enorme disperción usando un logaritmo
dataset[, ctrx_quarter_2 := log(ctrx_quarter + 1)]
quantile(dataset$ctrx_quarter_2, probs = c(0,0.5, 0.75, 0.9, 0.95, 0.99, 1))

# Comparemos dos splits
modelo_cq_1 <- rpart(clase_binaria ~ ctrx_quarter,
                    data = dataset,
                    xval = 0,
                    cp = -1,
                    maxdepth = 1)
modelo_cq_2 <- rpart(clase_binaria ~ ctrx_quarter_2,
                    data = dataset,
                    xval = 0,
                    cp = -1,
                    maxdepth = 1)

print(modelo_cq_1)
print(modelo_cq_2)

## Preguntas
## - Mirando los puntos de corte de los dos modelos ¿Existe una relación
##   matermática entre ellos?
## - ¿Es útil una transformación monótona en los árboles de decisión?

## ---------------------------
## Step 6: Data drifting.
## ---------------------------

# Actividad en clase:
# -------------------
# Qué es y como lo solucionamos
