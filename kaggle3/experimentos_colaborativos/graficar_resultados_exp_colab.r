
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
library(ggplot2)
library(knitr)
library(dplyr)
library(kableExtra)



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
if (grepl("windows", tolower(Sys.info()["sysname"]))) {
    path <- "C:/Users/maico/Documents/Mestrado/dmeyf2023/kaggle3/experimentos_colaborativos/exp/"
} else if (grepl("darwin", tolower(Sys.info()["sysname"]))) {
    path <- "/Users/maiconfialho/Documents/Mestrado/2023-2/dmeyf2023/kaggle3/experimentos_colaborativos/exp/"
}

setwd(path)

#fread com index
df_ganancias_sin_input_roto <- fread("./KA8240_ec_sin_input_roto/KA8240_ec_sin_input_roto_ganancias_semillerio.csv", key = "V1", stringsAsFactors = TRUE)
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

df_ganancia_sin_input_roto_sem <- df_ganancias_sin_input_roto[, mean(ganancia), by = semilla]
setnames(df_ganancia_sin_input_roto_sem, "V1", "ganancia_media")


combined_df_sem <- rbind(
  cbind(df_ganancia_sin_input_sem, experimento = "Sin Input - Lightgbm"),
  cbind(df_ganancias_input_media_sem, experimento = "Media Inputada"),
  cbind(df_ganancias_input_mediana_sem, experimento = "Mediana Inputada"),
  cbind(df_ganancia_sin_input_roto_sem, experimento = "Sin Input - roto")
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

df_ganancia_sin_input_roto_env <- df_ganancias_sin_input_roto[, mean(ganancia), by = envios]
setnames(df_ganancia_sin_input_roto_env, "V1", "ganancia_media")

combined_df_env <- rbind(
  cbind(df_ganancia_sin_input_env, experimento = "Sin Input - Lightgbm"),
  cbind(df_ganancias_input_media_env, experimento = "Media Inputada"),
  cbind(df_ganancias_input_mediana_env, experimento = "Mediana Inputada"),
  cbind(df_ganancia_sin_input_roto_env, experimento = "Sin Input - roto")
)


#combined_df_env<- combined_df_env[order(envios)]
#combined_df_env[, envios := .GRP, by = envios]
combined_df_env <- combined_df_env[order(experimento, envios)]
combined_df_env$envios <- factor(combined_df_env$envios, levels = unique(combined_df_env$envios))

# Gráfico
ggplot(combined_df_env, aes(x = envios, y = ganancia_media, color = experimento)) +
  geom_line(size = 1, aes(group = experimento)) + 
  geom_point(size = 4, aes(group = experimento)) +
  labs(title = "Comparación de Ganancias en cuatro Experimentos - Ganancia média por envio",
        x = "Envios",
        y = "Ganancia Media") +
  theme_minimal() +
  scale_color_manual(values = c("Sin Input - Lightgbm" = "blue", "Media Inputada" = "red", "Mediana Inputada" = "green", "Sin Input - roto" = "purple")) +
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
  labs(title = "Comparación de Ganancias en Tres Experimentos - Ganancia média por semilla",
        x = "Semilla",
        y = "Ganancia Media") +
  theme_minimal() +
  scale_color_manual(values = c("Sin Input - Lightgbm" = "blue", "Media Inputada" = "red", "Mediana Inputada" = "green", "Sin Input - roto" = "purple")) +
  theme(
    axis.title.y = element_text(size = 12),  # Ajustar el tamaño del título del eje y
    legend.position = "top",  # Mover la leyenda hacia arriba
    plot.title = element_text(hjust = 0.5)  # Centralizar el título del gráfico
  ) +
  scale_y_continuous(labels = scales::comma)  # Quitar la notación científica y agregar separadores de miles al eje y

tabla_ganancia_sin_input <- dcast(df_ganancias_sin_input, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_sin_input)[2:ncol(tabla_ganancia_sin_input)] <- paste0("semilla", 1:(ncol(tabla_ganancia_sin_input)-1))
#View(tabla_ganancia_sin_input)

tabla_ganacia_sin_input_roto <- dcast(df_ganancias_sin_input_roto, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganacia_sin_input_roto)[2:ncol(tabla_ganacia_sin_input_roto)] <- paste0("semilla", 1:(ncol(tabla_ganacia_sin_input_roto)-1))

tabla_ganancia_media <- dcast(df_ganancias_input_media, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_media)[2:ncol(tabla_ganancia_media)] <- paste0("semilla", 1:(ncol(tabla_ganancia_media)-1))
#View(tabla_ganancia_media)

tabla_ganancia_mediana <- dcast(df_ganancias_input_mediana, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_mediana)[2:ncol(tabla_ganancia_mediana)] <- paste0("semilla", 1:(ncol(tabla_ganancia_mediana)-1))
#View(tabla_ganancia_mediana)


tabla_kable_sin_input <- kable(tabla_ganancia_sin_input, "html") %>%
  kable_styling()  %>% add_header_above(c("Catastrophe Analysis sin Inputar - Lightgbm" = 21), escape = FALSE)
print(tabla_kable_sin_input)

tabla_kable_sin_input_roto <- kable(tabla_ganacia_sin_input_roto, "html") %>%
  kable_styling()  %>% add_header_above(c("Catastrophe Analysis sin Inputar - Roto" = 21), escape = FALSE)
print(tabla_kable_sin_input_roto)

tabla_kable_media <- kable(tabla_ganancia_media, "html") %>%
  kable_styling()  %>% add_header_above(c("Catastrophe Analysis con Media Inputada" = 21), escape = FALSE)
print(tabla_kable_media)

tabla_kable_mediana <- kable(tabla_ganancia_mediana, "html") %>%
  kable_styling()  %>% add_header_above(c("Catastrophe Analysis con Mediana Inputada" = 21), escape = FALSE)
print(tabla_kable_mediana)

#########
df_gan_sem_si <- aggregate(ganancia ~ semilla, data = df_ganancias_sin_input, FUN = mean)
df_gan_imedia <- aggregate(ganancia ~ semilla, data = df_ganancias_input_media, FUN = mean)
df_gan_imediana <- aggregate(ganancia ~ semilla, data = df_ganancias_input_mediana, FUN = mean)

wilcox.test(df_ganancias_sin_input$ganancia, df_ganancias_input_media$ganancia)

wilcox.test(df_ganancias_sin_input$ganancia, df_ganancias_input_mediana$ganancia)

wilcox.test(df_ganancias_sin_input$ganancia, df_ganancias_sin_input_roto$ganancia)

wilcox.test(df_ganancias_input_media$ganancia, df_ganancias_sin_input_roto$ganancia)
