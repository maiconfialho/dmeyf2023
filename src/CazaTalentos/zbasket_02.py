import  numpy as np

np.random.seed(102191)

# calcula cuantos encestes logra un jugadora con indice de enceste prob
# haciendo qyt tiros libres

def ftirar(prob, qty):
  return sum(np.random.rand(qty) < prob)



# defino los jugadoras
mejor = 0.7
peloton = np.array(range(501, 600)) / 1000
jugadoras = np.append(mejor, peloton)

# veo que tiene el vector
jugadoras

# vectorizo la funcion  ftirar
vec_ftirar = np.vectorize(ftirar)

# hago que las 100 jugadoras tiren 10 veces cada una
vec_ftirar(jugadoras, 10)

primera_ganadora = 0

for i in range(10000): # diez mil experimentos
  vaciertos = vec_ftirar(jugadoras, 10) # 10 tiros libres cada jugadora
  mejor_ronda = np.argmax(vaciertos)
  if mejor_ronda == 0:
    primera_ganadora += 1




print(primera_ganadora)
