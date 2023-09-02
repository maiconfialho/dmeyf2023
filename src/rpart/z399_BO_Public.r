# Optimizacion Bayesiana de hiperparametros de  rpart
# que va directamente contra el Public Leaderboard
# este script AUN no entrena en un dataset con oversampling de los BAJA+2

# dedicado a Federico Idoeta, Impossible is Nothing,  02-sep-2022


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


# Defino la  Optimizacion Bayesiana
PARAM <- list()
PARAM$experimento <- "HT3990"

# cantidad de iteraciones de la Optimizacion Bayesiana
PARAM$BO_iter <- 24  # iteraciones inteligentes   24= 40 - 4*4

#  de los hiperparametros
PARAM$hs <- makeParamSet(
  makeIntegerParam("minsplit", lower = 500L, upper = 1500L),
  makeIntegerParam("minbucket", lower = 200L, upper = 800L),
  makeIntegerParam("maxdepth", lower = 6L, upper = 12L),
  makeIntegerParam("corte", lower = 8000L, upper = 10000L),
  forbidden = quote(minbucket > 0.5 * minsplit)
)
# minbuket NO PUEDE ser mayor que la mitad de minsplit

PARAM$semilla_azar <- 270001 # primer semilla de Federico

#------------------------------------------------------------------------------

leer_numero <- function( mensaje ) {
  res <- readline( mensaje )
  while( is.na( as.numeric( res ))) {
    cat( "Debe introducit un numero, el separador decimal es la coma\n" )
    res <- readline( mensaje )
  }

  return( as.numeric(res) )
}
#------------------------------------------------------------------------------

leer_verificado <- function( mensaje ) {
  repeat {
  
    n1 <- leer_numero( mensaje )
    cat( "Por favor, vuelva a cargar el mismo numero\n" )
    n2 <- leer_numero( mensaje )

   if( n1 != n2 )  cat( "Los numeros no coinciden, debe volver a cargar\n\n" )
   if( n1== n2 ) break
  }

  return( n1 )
}
#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(reg, arch = NA, folder = "./work/", ext = ".txt",
                    verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  # Escribo los titulos
  if (!file.exists(archivo)) {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )

    cat(linea, file = archivo)
  }

  # la fecha y hora
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
    gsub(", ", "\t", toString(reg)), "\n"
  )

  # grabo al archivo
  cat(linea, file = archivo, append = TRUE)

  # imprimo por pantalla
  if (verbose) cat(linea)
}
#----------------------------------------------------------------------------
# param tiene los hiperparametros del arbol

ArbolSimple <- function( data, param, iteracion) {

  param2 <- copy( param )
  param2$cp <- -1
  param2$minsplit <- param$minsplit 
  param2$minbucket <- param$minbucket
  param2$corte <- param$corte

  modelo <- rpart("clase_binaria ~ . - clase_ternaria",
    data = dtrain,
    xval = 0,
    control = param2
  )

  # aplico el modelo a los datos de testing
  # aplico el modelo sobre los datos de testing
  # quiero que me devuelva probabilidades
  prediccion <- predict(modelo,
    dapply,
    type = "prob"
  )

  # esta es la probabilidad de baja
  prob_baja <- prediccion[, "POS"]

  tablita <- copy( dapply[, list(numero_de_cliente) ] )
  tablita[ , prob := prob_baja ]
  setorder( tablita, -prob )

  # grabo el submit a Kaggle
  tablita[ , Predicted := 0L ]
  tablita[ 1:param2$corte, Predicted := 1L ]

  nom_submit <- paste0("z399_", sprintf( "%03d", iteracion ), ".csv" )
  fwrite( tablita[ , list(numero_de_cliente, Predicted)],
          file= nom_submit,
          sep= "," )

  # solicito que el humano a cargo ingrese la ganancia publica
  mensaje <- paste0( "haga el submit a Kaggle de ", nom_submit,
                     " y cargue la ganancia publica : " )

  ganancia_public <- leer_verificado( mensaje )

  return(ganancia_public)
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia <- function(x) {
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1

  # x los hiperparametros del arbol
  ganancia_public <- ArbolSimple( dtrain, x, GLOBAL_iteracion )

  # logueo
  xx <- x
  xx$cp <- -1
  xx$ganancia <- ganancia_public
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = archivo_log)

  # para que mlrMBO tenga todo reseteado
  set.seed( PARAM$semilla_azar )

  return(ganancia_public)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Establezco el Working Directory
setwd("~/buckets/b1/")

# cargo los datos
dataset <- fread("./datasets/competencia_01.csv")

# defino la clase_binaria2
dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]

dtrain <- dataset[foto_mes==202103]
dapply <- dataset[foto_mes==202105]

# creo la carpeta donde va el experimento
#  HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create( paste0("./exp/", PARAM$experimento, "/"), 
           showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd( paste0("./exp/", PARAM$experimento, "/") )


# en estos archivos quedan los resultados
archivo_log <- "BO_log.txt"
archivo_BO <- "bayesian.RDATA"

# leo si ya existe el log
#  para retomar en caso que se se corte el programa
GLOBAL_iteracion <- 0

if (file.exists(archivo_log)) {
  tabla_log <- fread(archivo_log)
  GLOBAL_iteracion <- nrow(tabla_log)
}



# Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar <- EstimarGanancia

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,
#  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
# minimize= FALSE estoy Maximizando la ganancia
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar,
  minimize = FALSE,
  noisy = TRUE,
  par.set = PARAM$hs,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl(
  save.on.disk.at.time = 600,
  save.file.path = archivo_BO
)

ctrl <- setMBOControlTermination(ctrl, iters = PARAM$BO_iter)
ctrl <- setMBOControlInfill(ctrl,  crit = makeMBOInfillCritEI())

surr.km <- makeLearner("regr.km",
  predict.type = "se",
  covtype = "matern3_2", control = list(trace = TRUE)
)


# para que mlrMBO tenga todo reseteado
set.seed( PARAM$semilla_azar )

# inicio la optimizacion bayesiana
if (!file.exists(archivo_BO)) {
  run <- mbo(
    fun = obj.fun,
    learner = surr.km,
    control = ctrl
  )
} else {
  run <- mboContinue(archivo_BO)
}
# retomo en caso que ya exista
