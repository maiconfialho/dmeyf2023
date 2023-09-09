# Aplicacion de modelo

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


# cargo las librerias que necesito
require("data.table")
require("rpart")


# los parametros del script
#  deben copiarse a mano
PARAM <- list()

PARAM$experimento <- "KA4000-01"

# puede ser { "ternaria", "binaria1", "binaria2" }
PARAM$clase <- "binaria2"

# poner 0 si no se quiere que se haga oversampling
PARAM$oversampling <- 100

# poner algo mayor a cero si se quiere cortar por probabilidad
#  generalmente debe ser 0.025 , si se quiere cortar por probabilidad
PARAM$prob_corte <-  -1

# poner -1 si se quiere cortar por probabilidad
#  un numero mayor o igual a cero indica que se corta por cant envios
PARAM$corte <- 10000

# estos valores son simplemente de ejemplo
PARAM$rpart$cp <- -1
PARAM$rpart$minsplit <- 2500
PARAM$rpart$minbucket <- 250
PARAM$rpart$maxdepth <- 6

#------------------------------------------------------------------------------
#Aqui comienza el programa


# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory


if( PARAM$prob_corte <= 0 & PARAM$corte <= 0 ) stop("debe elegir al menos una opcion de corte" )
if( PARAM$prob_corte > 0 & PARAM$corte > 0 ) stop("No puede elegir dos opcinoes de corte al mismo tiempo" )
if( PARAM$clase == "binaria2" & PARAM$corte <= 0 ) stop( "en el caso de clase binaria2  debe cortar por envios, PARAM$corte" )

# cargo el dataset
dataset <- fread("./datasets/competencia_01.csv")

# creo la carpeta donde va el experimento
#  HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create( paste0("./exp/", PARAM$experimento, "/"),
           showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd( paste0("./exp/", PARAM$experimento, "/") )

switch(PARAM$clase,
  "ternaria"  = dataset[, clase_nueva := ifelse( clase_ternaria=="BAJA+2", "POS", clase_ternaria ) ],
  "binaria1"  = dataset[, clase_nueva := ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" ) ],
  "binaria2"  = dataset[, clase_nueva := ifelse( clase_ternaria %in% c( "BAJA+1","BAJA+2"), "POS", "NEG" ) ],
  stop(" PARAM$clase debe tener un valor valido")
)



dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar

dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo
dapply[, clase_ternaria := NA ]


# hago el oversampling si hace falta
vector_pesos <- rep( 1.0, nrow(dtrain) )
if( PARAM$oversampling > 0 )   
  vector_pesos <- dtrain[, ifelse( clase_nueva=="POS", PARAM$oversampling, 1) ]

# genero el modelo,  aqui se construye el arbol
modelo <- rpart(
        formula = "clase_nueva ~ . -clase_ternaria",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        control = PARAM$rpart,
        weights =  vector_pesos
)



# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)


# calculo en tablita lo necesario para generar el archivo prediccion
tablita <- dapply[, list(numero_de_cliente) ]
tablita[, prob := prediccion[, "POS"] ]
setorder( tablita, -prob )
tablita[, Predicted := 0L ]

if( PARAM$prob_corte > 0 ) {
  tablita[, Predicted := as.numeric(prob > PARAM$prob_corte)]
} else {
  if( PARAM$corte > 0 ) {
    tablita[ 1:PARAM$corte, Predicted := 1]
  }
}


# genero el submit para Kaggle
nom_archivo <- paste0( PARAM$experimento, "_kaggle.csv" )

fwrite(tablita[, list(numero_de_cliente, Predicted)],
        file = nom_archivo,
        sep = ","
)
