# Script para encontrar Visuamente  el data drifting
# focalizado solo en los campos de un buen arbol de deicision


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("RColorBrewer")

PARAM <- list()

PARAM$experimento <- "DR5940"

PARAM$dataset <- "competencia_02.csv.gz"

PARAM$meses <- c(202104, 202105, 202107)

#------------------------------------------------------------------------------

graficar_campo <- function(campo) {
  qty <- length(PARAM$meses)
  colores <- brewer.pal(n = qty, name = "Dark2")
  # quito de grafico las colas del 5% de las densidades
  tbl <- dataset[
    , list("tile" = quantile(get(campo), prob = c(0.05, 0.95), na.rm = TRUE)),
    foto_mes
  ]
  xxmin <- tbl[, min(tile)]
  xxmax <- tbl[, max(tile)]


  densidad <- density(dataset[foto_mes == PARAM$meses[qty], get(campo)],
    kernel = "gaussian", na.rm = TRUE
  )

  plot(densidad,
    col = colores[qty],
    xlim = c(xxmin, xxmax),
    ylim = c(0, pmax(max(densidad$y), max(densidad$y))),
    main = campo,
    lty = 2
  )

  for (i in (qty - 1):1) {
    densidad <- density(dataset[foto_mes == PARAM$meses[i], get(campo)],
      kernel = "gaussian", na.rm = TRUE
    )

    lines(densidad, col = colores[i], lty = 1)
  }

  legend("topright",
    legend = PARAM$meses,
    col = colores,
    lty = c(rep(1, qty - 1), 2)
  )
}
#------------------------------------------------------------------------------
# Aqui comienza el programa

if (length(PARAM$meses) < 2) {
  stop("deben haber al menos DOS meses en PARAM$meses\n")
}

setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar
dataset <- fread(paste0("./datasets/", PARAM$dataset))

dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0("./exp/", PARAM$experimento, "/"))

dataset <- dataset[foto_mes %in% PARAM$meses]
gc()


campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_clientes", "foto_mes", "clase_ternaria")
)



# genero los graficos, uno por hoja
pdf("densidades.pdf")

for (campo in campos_buenos) {
  cat(campo, "  ")
  graficar_campo(campo)
}

dev.off()
