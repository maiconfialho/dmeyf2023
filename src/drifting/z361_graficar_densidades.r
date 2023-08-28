# Script para encontrar Visuamente  el data drifting
# focalizado solo en los campos de un buen arbol de deicision

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")


#------------------------------------------------------------------------------

graficar_campo <- function(campo) {
  # quito de grafico las colas del 5% de las densidades
  qA <- quantile(dataset[foto_mes == 202103, get(campo)],
    prob = c(0.05, 0.95), na.rm = TRUE
  )

  qB <- quantile(dataset[foto_mes == 202105, get(campo)],
    prob = c(0.05, 0.95), na.rm = TRUE
  )

  xxmin <- pmin(qA[[1]], qB[[1]])
  xxmax <- pmax(qA[[2]], qB[[2]])

  densidad_A <- density(dataset[foto_mes == 202103, get(campo)],
    kernel = "gaussian", na.rm = TRUE
  )

  densidad_B <- density(dataset[foto_mes == 202105, get(campo)],
    kernel = "gaussian", na.rm = TRUE
  )

  plot(densidad_A,
    col = "blue",
    xlim = c(xxmin, xxmax),
    ylim = c(0, pmax(max(densidad_A$y), max(densidad_B$y))),
    main = campo
  )

  lines(densidad_B, col = "red", lty = 2)

  legend("topright",
    legend = c("202107", "202109"),
    col = c("blue", "red"), lty = c(1, 2)
  )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar
dataset <- fread("./datasets/competencia_01.csv")

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/DR3610/", showWarnings = FALSE)
setwd("./exp/DR3610/")

dataset <- dataset[foto_mes %in% c(202103, 202105)]

# creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[
  foto_mes == 202103,
  clase_binaria := ifelse(clase_ternaria == "CONTINUA", "NO", "SI")
]

# Entreno el modelo
# utilizo los mejores hiperparametros encontrados
# en una Bayesian Optimizationcon 5-fold Cross Validation
modelo <- rpart(
  formula = "clase_binaria ~ . -clase_ternaria",
  data = dataset[foto_mes == 202103], # los datos donde voy a entrenar
  xval = 0,
  cp = -0.67,
  minsplit = 1144,
  minbucket = 539,
  maxdepth = 8
)


campos_modelo <- names(modelo$variable.importance)
campos_buenos <- c(campos_modelo, setdiff(colnames(dataset), campos_modelo))
campos_buenos <- setdiff(
  campos_buenos,
  c("foto_mes", "clase_ternaria", "clase_binaria")
)



pdf("densidades_03_05.pdf")

for (campo in campos_buenos) {
  cat(campo, "  ")
  graficar_campo(campo)
}

dev.off()
