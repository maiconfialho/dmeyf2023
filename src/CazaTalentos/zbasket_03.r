

set.seed(102191)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


# defino las jugadoras
mejor <- 0.7
peloton <- (501:599) / 1000
jugadoras <- c(mejor, peloton)

# veo que tiene el vector
jugadoras




for (i in 1:10) {
  vaciertos <- mapply(ftirar, jugadoras, 10) # cada jugadora tira 10 tiros
  mejor_ronda <- which.max(vaciertos)
  aciertos_torneo <- vaciertos[mejor_ronda]

  aciertos_segunda <- ftirar(jugadoras[mejor_ronda], 10)

  cat(aciertos_torneo, "\t", aciertos_segunda, "\n")
}
