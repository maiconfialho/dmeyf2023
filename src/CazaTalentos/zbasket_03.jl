using Random

Random.seed!(102191)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# que hace qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end


# defino las jugadoras
mejor = [0.7]
peloton = Vector((501:599) / 1000)
jugadoras = append!(mejor, peloton)

# veo que tiene el vector
jugadoras




for i = 1:10
  vaciertos = ftirar.(jugadoras, 10)  # 10 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)[2]
  aciertos_torneo = vaciertos[mejor_ronda]

  aciertos_segunda = ftirar.(jugadoras[mejor_ronda], 10)

  println(aciertos_torneo, "\t", aciertos_segunda)
end

