library(dplyr)
library(ggplot2)

df_cazatalentos1 <- data.frame(
  Cazatalentos = rep("Cazatalentos 1", times = 5),
  Candidata = c("A", "B", "C", "D", "E"),
  Aciertos = c(80, 79, 79, 78, 78)
)

df_cazatalentos2 <- data.frame(
  Cazatalentos = rep("Cazatalentos 2", times = 5),
  Candidata = c("A", "B", "C", "D", "E"),
  Aciertos = c(80, 79, 79, 79, 79)
)

df_cazatalentos3 <- data.frame(
  Cazatalentos = rep("Cazatalentos 3", times = 2),
  Candidata = c("A", "B"),
  Aciertos = c(80, 75)
)

df_cazatalentos4 <- data.frame(
  Cazatalentos = rep("Cazatalentos 4", times = 1),
  Candidata = c("A"),
  Aciertos = c(9)
)

df_cazatalentos5 <- data.frame(
  Cazatalentos = rep("Cazatalentos 5", times = 5),
  Candidata = c("A", "B", "C", "D", "E"),
  Aciertos1 = c(85, 84, 84, 82, 81),
  Aciertos2 = c(69, 74, 74, 70, 75),
  Aciertos3 = c(70, 76, 75, 73, 74)
)

df_cazatalentos6 <- data.frame(
  Cazatalentos = rep("Cazatalentos 6", times = 1),
  Candidata = c("A"),
  Aciertos = c(80)
)

df_cazatalentos7 <- data.frame(
  Cazatalentos = rep("Cazatalentos 7", times = 5),
  Candidata = c("A", "B", "C", "D", "E"),
  Aciertos2 = c(80, 79, 78, 77, 72)
)

df_cazatalentos8 <- data.frame(
  Cazatalentos = rep("Cazatalentos 8", times = 5),
  Candidata = c("A", "B", "C", "D", "E"),
  Aciertos = c(85, 84, 84, 83, 83)
)

df_cazatalentos9 <- data.frame(
  Cazatalentos = rep("Cazatalentos 9", times = 1),
  Candidata = c("A"),
  Aciertos = c(68, 74, 78, 70, 68, 63, 80, 68, 67, 65)
)

#Apenas a cazatalentos5 disponibilizou dados para estudarmos
# Calcular a média e a variância dos jogadores do Cazatalentos 5
media_jogadores5 <- rowMeans(df_cazatalentos5[, 3:5], na.rm = TRUE)
variancia_jogadores5 <- apply(df_cazatalentos5[, 3:5], 1, var, na.rm = TRUE)

# Exibir os resultados
print("Média dos jogadores do Cazatalentos 5:")
print(media_jogadores5)

print("Variância dos jogadores do Cazatalentos 5:")
print(variancia_jogadores5)

#A cazatalentos 9 nao disponibilizou os dados, mas os temos para fazer uma análise
# Calcular a média e a variância dos jogadores do Cazatalentos 9
media_jogadores9 <- mean(df_cazatalentos9$Aciertos)
variancia_jogadores9 <- var(df_cazatalentos9$Aciertos)

# Exibir os resultados
print("Média dos jogadores do Cazatalentos 9:")
print(media_jogadores9)

print("Variância dos jogadores do Cazatalentos 9:")
print(variancia_jogadores9)
