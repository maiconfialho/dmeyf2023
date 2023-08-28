import  numpy as np

np.random.seed(102191)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# que hace qyt tiros libres

def ftirar(prob, qty):
  return sum(np.random.rand(qty) < prob)



# defino las jugadoras
mejor = 0.7
peloton = np.array(range(501, 600)) / 1000
jugadoras = np.append(mejor, peloton)

# veo que tiene el vector
jugadoras

# vectorizo la funcion  ftirar
vec_ftirar = np.vectorize(ftirar)

for i in range(10):
  vaciertos = vec_ftirar(jugadoras, 10) # 10 tiros libres cada jugadora
  mejor_ronda = np.argmax(vaciertos)
  aciertos_torneo = vaciertos[mejor_ronda]
  aciertos_segunda = vec_ftirar(jugadoras[mejor_ronda], 10)
  print(aciertos_torneo, "\t", aciertos_segunda)


