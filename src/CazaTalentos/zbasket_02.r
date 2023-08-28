

set.seed(102191)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


# defino los jugadores
mejor <- 0.7
peloton <- (501:599) / 1000
jugadoras <- c(mejor, peloton)

# veo que tiene el vector
jugadoras




# hago que los 100 jugadores tiren 10 veces cada uno
mapply(ftirar, jugadoras, 10)

primera_ganadora <- 0

for (i in 1:10000) { # diez mil experimentos

  vaciertos <- mapply(ftirar, jugadoras, 10) # 10 tiros libres cada jugador

  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}


print(primera_ganadora)
