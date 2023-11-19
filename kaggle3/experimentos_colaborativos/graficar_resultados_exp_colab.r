
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
library(ggplot2)



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
if (grepl("windows", tolower(Sys.info()["sysname"]))) {
    path <- "C:/Users/maico/Documents/Mestrado/dmeyf2023/kaggle3/exp/"
} else if (grepl("darwin", tolower(Sys.info()["sysname"]))) {
    path <- "/Users/maiconfialho/Documents/Mestrado/2023-2/dmeyf2023/kaggle3/exp/"
}

setwd(path)

#fread com index

df_ganancias_sin_input <- fread("./KA8240_ec_sin_input/KA8240_ec_sin_input_ganancias_semillerio.csv", key = "V1", stringsAsFactors = TRUE)
df_ganancias_input_media <- fread("./KA8240_ec_input_media/KA8240_ec_input_media_ganancias_semillerio.csv", key = "V1", stringsAsFactors = TRUE)
df_ganancias_input_mediana <- fread("./KA8240_ec_input_mediana/KA8240_ec_input_mediana_ganancias_semillerio.csv", key = "V1", stringsAsFactors = TRUE)


#calcular a media da ganancia agrupada por semilla
df_ganancia_sin_input_sem <- df_ganancias_sin_input[, mean(ganancia), by = semilla]
setnames(df_ganancia_sin_input_sem, "V1", "ganancia_media")

df_ganancias_input_media_sem <- df_ganancias_input_media[, mean(ganancia), by = semilla]
setnames(df_ganancias_input_media_sem, "V1", "ganancia_media")

df_ganancias_input_mediana_sem <- df_ganancias_input_mediana[, mean(ganancia), by = semilla]
setnames(df_ganancias_input_mediana_sem, "V1", "ganancia_media")


combined_df_sem <- rbind(
  cbind(df_ganancia_sin_input_sem, experimento = "Sin Input"),
  cbind(df_ganancias_input_media_sem, experimento = "Media Inputada"),
  cbind(df_ganancias_input_mediana_sem, experimento = "Mediana Inputada")
)

combined_df_sem <- combined_df_sem[order(semilla)]
combined_df_sem[, semilla := .GRP, by = semilla]
combined_df_sem <- combined_df_sem[order(experimento, semilla)]
combined_df_sem$semilla <- factor(combined_df_sem$semilla, levels = unique(combined_df_sem$semilla))

#calcular a media da ganancia agrupada por envios 
df_ganancia_sin_input_env <- df_ganancias_sin_input[, mean(ganancia), by = envios]
setnames(df_ganancia_sin_input_env, "V1", "ganancia_media")

df_ganancias_input_media_env <- df_ganancias_input_media[, mean(ganancia), by = envios]
setnames(df_ganancias_input_media_env, "V1", "ganancia_media")

df_ganancias_input_mediana_env <- df_ganancias_input_mediana[, mean(ganancia), by = envios]
setnames(df_ganancias_input_mediana_env, "V1", "ganancia_media")

combined_df_env <- rbind(
  cbind(df_ganancia_sin_input_env, experimento = "Sin Input"),
  cbind(df_ganancias_input_media_env, experimento = "Media Inputada"),
  cbind(df_ganancias_input_mediana_env, experimento = "Mediana Inputada")
)


#combined_df_env<- combined_df_env[order(envios)]
#combined_df_env[, envios := .GRP, by = envios]
combined_df_env <- combined_df_env[order(experimento, envios)]
combined_df_env$envios <- factor(combined_df_env$envios, levels = unique(combined_df_env$envios))

# Gráfico
ggplot(combined_df_env, aes(x = envios, y = ganancia_media, color = experimento)) +
  geom_line(size = 1, aes(group = experimento)) + 
  geom_point(size = 4, aes(group = experimento)) +
  labs(title = "Comparación de Ganancias en Tres Experimentos - Ganancia média por semilla",
       x = "Envios",
       y = "Ganancia Media") +
  theme_minimal() +
  scale_color_manual(values = c("Sin Input" = "blue", "Media Inputada" = "red", "Mediana Inputada" = "green")) +
  theme(
    axis.title.y = element_text(size = 12),  # Ajustar el tamaño del título del eje y
    legend.position = "top",  # Mover la leyenda hacia arriba
    plot.title = element_text(hjust = 0.5)  # Centralizar el título del gráfico
  ) +
  scale_y_continuous(labels = scales::comma) 


#graficar por semilla
ggplot(combined_df_sem, aes(x = semilla, y = ganancia_media, color = experimento)) +
  geom_line(size = 1, aes(group = experimento)) + 
  geom_point(size = 4, aes(group = experimento)) +
  labs(title = "Comparación de Ganancias en Tres Experimentos - Ganancia média por envio",
       x = "Semilla",
       y = "Ganancia Media") +
  theme_minimal() +
  scale_color_manual(values = c("Sin Input" = "blue", "Media Inputada" = "red", "Mediana Inputada" = "green")) +
  theme(
    axis.title.y = element_text(size = 12),  # Ajustar el tamaño del título del eje y
    legend.position = "top",  # Mover la leyenda hacia arriba
    plot.title = element_text(hjust = 0.5)  # Centralizar el título del gráfico
  ) +
  scale_y_continuous(labels = scales::comma)  # Quitar la notación científica y agregar separadores de miles al eje y

tabla_ganancia_sin_input <- dcast(df_ganancias_sin_input, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_sin_input)[2:ncol(tabla_ganancia_sin_input)] <- paste0("semilla", 1:(ncol(tabla_ganancia_sin_input)-1))
View(tabla_ganancia_sin_input)

tabla_ganancia_media <- dcast(df_ganancias_input_media, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_media)[2:ncol(tabla_ganancia_media)] <- paste0("semilla", 1:(ncol(tabla_ganancia_media)-1))
View(tabla_ganancia_media)

tabla_ganancia_mediana <- dcast(df_ganancias_input_mediana, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_mediana)[2:ncol(tabla_ganancia_mediana)] <- paste0("semilla", 1:(ncol(tabla_ganancia_mediana)-1))
View(tabla_ganancia_mediana)
