
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
df_ganancias_input_media_total <- fread("./KA8240_ec_input_media_total/KA8240_ec_input_media_total_ganancias_semillerio.csv", key = "V1", stringsAsFactors = TRUE)
df_ganancias_input_mediana_total <- fread("./KA8240_ec_input_mediana_total/KA8240_ec_input_mediana_total_ganancias_semillerio.csv", key = "V1", stringsAsFactors = TRUE)


#calcular a media da ganancia agrupada por semilla
df_ganancia_sin_input_sem <- df_ganancias_sin_input[, mean(ganancia), by = semilla]
setnames(df_ganancia_sin_input_sem, "V1", "ganancia_media")

df_ganancias_input_media_sem <- df_ganancias_input_media[, mean(ganancia), by = semilla]
setnames(df_ganancias_input_media_sem, "V1", "ganancia_media")

df_ganancias_input_mediana_sem <- df_ganancias_input_mediana[, mean(ganancia), by = semilla]
setnames(df_ganancias_input_mediana_sem, "V1", "ganancia_media")

df_ganancia_sin_input_roto_sem <- df_ganancias_sin_input_roto[, mean(ganancia), by = semilla]
setnames(df_ganancia_sin_input_roto_sem, "V1", "ganancia_media")

df_ganancias_input_media_total_sem <- df_ganancias_input_media_total[, mean(ganancia), by = semilla]
setnames(df_ganancias_input_media_total_sem, "V1", "ganancia_media")

df_ganancias_input_mediana_total_sem <- df_ganancias_input_mediana_total[, mean(ganancia), by = semilla]
setnames(df_ganancias_input_mediana_total_sem, "V1", "ganancia_media")



combined_df_sem <- rbind(
  cbind(df_ganancia_sin_input_sem, experimento = "Sin Input - Lightgbm"),
  cbind(df_ganancias_input_media_sem, experimento = "Media Inputada"),
  cbind(df_ganancias_input_mediana_sem, experimento = "Mediana Inputada"),
  cbind(df_ganancias_input_media_total_sem, experimento = "Media Inputada - Total"),
  cbind(df_ganancias_input_mediana_total_sem, experimento = "Mediana Inputada - Total"),
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

df_ganancias_input_media_total_env <- df_ganancias_input_media_total[, mean(ganancia), by = envios]
setnames(df_ganancias_input_media_total_env, "V1", "ganancia_media")

df_ganancias_input_mediana_total_env <- df_ganancias_input_mediana_total[, mean(ganancia), by = envios]
setnames(df_ganancias_input_mediana_total_env, "V1", "ganancia_media")

combined_df_env <- rbind(
  cbind(df_ganancia_sin_input_env, experimento = "Sin Input - Lightgbm"),
  cbind(df_ganancias_input_media_env, experimento = "Media Inputada"),
  cbind(df_ganancias_input_mediana_env, experimento = "Mediana Inputada"),
  cbind(df_ganancias_input_media_total_env, experimento = "Media Inputada - Total"),
  cbind(df_ganancias_input_mediana_total_env, experimento = "Mediana Inputada - Total"),
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
  scale_color_manual(values = c("Sin Input - Lightgbm" = "blue", "Media Inputada" = "red", 
                                "Mediana Inputada" = "green", "Sin Input - roto" = "purple",
                                "Media Inputada - Total" = "black", "Mediana Inputada - Total" = "grey")) +
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
  scale_color_manual(values = c("Sin Input - Lightgbm" = "blue", "Media Inputada" = "red", 
                                "Mediana Inputada" = "green", "Sin Input - roto" = "purple",
                                "Media Inputada - Total" = "black", "Mediana Inputada - Total" = "grey")) +  theme(
    axis.title.y = element_text(size = 12),  # Ajustar el tamaño del título del eje y
    legend.position = "top",  # Mover la leyenda hacia arriba
    plot.title = element_text(hjust = 0.5)  # Centralizar el título del gráfico
  ) +
  scale_y_continuous(labels = scales::comma)  # Quitar la notación científica y agregar separadores de miles al eje y


tabla_ganancia_sin_input <- dcast(df_ganancias_sin_input, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_sin_input)[2:ncol(tabla_ganancia_sin_input)] <- paste0("semilla", 1:(ncol(tabla_ganancia_sin_input)-1))
tabla_ganancia_sin_input$ganancia_media <- rowMeans(tabla_ganancia_sin_input[, 2:ncol(tabla_ganancia_sin_input)])
tabla_ganancia_sin_input$ganancia_acumulada <- rowSums(tabla_ganancia_sin_input[, 2:ncol(tabla_ganancia_sin_input)])

tabla_ganancias_sin_input <- tabla_ganancia_sin_input[, c("envios", "ganancia_media", "ganancia_acumulada")]

###########
tabla_ganacia_sin_input_roto <- dcast(df_ganancias_sin_input_roto, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganacia_sin_input_roto)[2:ncol(tabla_ganacia_sin_input_roto)] <- paste0("semilla", 1:(ncol(tabla_ganacia_sin_input_roto)-1))
tabla_ganacia_sin_input_roto$ganancia_media <- rowMeans(tabla_ganacia_sin_input_roto[, 2:ncol(tabla_ganacia_sin_input_roto)])
tabla_ganacia_sin_input_roto$ganancia_acumulada <- rowSums(tabla_ganacia_sin_input_roto[, 2:ncol(tabla_ganacia_sin_input_roto)])

tabla_ganancias_sin_input_roto <- tabla_ganacia_sin_input_roto[, c("envios", "ganancia_media", "ganancia_acumulada")]

###############
tabla_ganancia_media <- dcast(df_ganancias_input_media, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_media)[2:ncol(tabla_ganancia_media)] <- paste0("semilla", 1:(ncol(tabla_ganancia_media)-1))
tabla_ganancia_media$ganancia_media <- rowMeans(tabla_ganancia_media[, 2:ncol(tabla_ganancia_media)])
tabla_ganancia_media$ganancia_acumulada <- rowSums(tabla_ganancia_media[, 2:ncol(tabla_ganancia_media)])

tabla_ganancias_media <- tabla_ganancia_media[, c("envios", "ganancia_media", "ganancia_acumulada")]

#####################
tabla_ganancia_mediana <- dcast(df_ganancias_input_mediana, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_mediana)[2:ncol(tabla_ganancia_mediana)] <- paste0("semilla", 1:(ncol(tabla_ganancia_mediana)-1))
tabla_ganancia_mediana$ganancia_media <- rowMeans(tabla_ganancia_mediana[, 2:ncol(tabla_ganancia_mediana)])
tabla_ganancia_mediana$ganancia_acumulada <- rowSums(tabla_ganancia_mediana[, 2:ncol(tabla_ganancia_mediana)])

tabla_ganancias_mediana <- tabla_ganancia_mediana[, c("envios", "ganancia_media", "ganancia_acumulada")]

##################
tabla_ganancia_mediana_total <- dcast(df_ganancias_input_mediana_total, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_mediana_total)[2:ncol(tabla_ganancia_mediana_total)] <- paste0("semilla", 1:(ncol(tabla_ganancia_mediana_total)-1))
tabla_ganancia_mediana_total$ganancia_media <- rowMeans(tabla_ganancia_mediana_total[, 2:ncol(tabla_ganancia_mediana_total)])
tabla_ganancia_mediana_total$ganancia_acumulada <- rowSums(tabla_ganancia_mediana_total[, 2:ncol(tabla_ganancia_mediana_total)])

tabla_ganancias_mediana_total <- tabla_ganancia_mediana_total[, c("envios", "ganancia_media", "ganancia_acumulada")]

#############
tabla_ganancia_media_total <- dcast(df_ganancias_input_media_total, envios ~ semilla, value.var = "ganancia")
colnames(tabla_ganancia_media_total)[2:ncol(tabla_ganancia_media_total)] <- paste0("semilla", 1:(ncol(tabla_ganancia_media_total)-1))
tabla_ganancia_media_total$ganancia_media <- rowMeans(tabla_ganancia_media_total[, 2:ncol(tabla_ganancia_media_total)])
tabla_ganancia_media_total$ganancia_acumulada <- rowSums(tabla_ganancia_media_total[, 2:ncol(tabla_ganancia_media_total)])

tabla_ganancias_media_total <- tabla_ganancia_media_total[, c("envios", "ganancia_media", "ganancia_acumulada")]


combined_df_ganancias <- rbind(
  cbind(tabla_ganancias_sin_input, experimento = "Sin Input - Lightgbm"),
  cbind(tabla_ganancias_sin_input_roto, experimento = "Sin Input - roto"),
  cbind(tabla_ganancias_media, experimento = "Media Inputada"),
  cbind(tabla_ganancias_mediana, experimento = "Mediana Inputada"),
  cbind(tabla_ganancias_media_total, experimento = "Media Inputada - Total"),
  cbind(tabla_ganancias_mediana_total, experimento = "Mediana Inputada - Total")
)

#graficar ganancias
ggplot(combined_df_ganancias, aes(x = envios, y = ganancia_media, color = experimento)) +
  geom_line(size = 1, aes(group = experimento)) + 
  geom_point(size = 4, aes(group = experimento)) +
  labs(title = "Comparación de Ganancias medias por envio",
       x = "Envios",
       y = "Ganancia Media") +
  theme_minimal() +
  scale_color_manual(values = c("Sin Input - Lightgbm" = "blue", "Media Inputada" = "red", 
                                "Mediana Inputada" = "green", "Sin Input - roto" = "purple",
                                "Media Inputada - Total" = "black", "Mediana Inputada - Total" = "grey")) +  theme(
                                  axis.title.y = element_text(size = 12),  # Ajustar el tamaño del título del eje y
                                  legend.position = "top",  # Mover la leyenda hacia arriba
                                  plot.title = element_text(hjust = 0.5)  # Centralizar el título del gráfico
                                ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = unique(combined_df_ganancias$envios))

#graficar ganancias
ggplot(combined_df_ganancias, aes(x = envios, y = ganancia_acumulada, color = experimento)) +
  geom_line(size = 1, aes(group = experimento)) + 
  geom_point(size = 4, aes(group = experimento)) +
  labs(title = "Comparación de Ganancias acumuladas por envio",
       x = "Envios",
       y = "Ganancia acumulada") +
  theme_minimal() +
  scale_color_manual(values = c("Sin Input - Lightgbm" = "blue", "Media Inputada" = "red", 
                                "Mediana Inputada" = "green", "Sin Input - roto" = "purple",
                                "Media Inputada - Total" = "black", "Mediana Inputada - Total" = "grey")) +  theme(
                                  axis.title.y = element_text(size = 12),  # Ajustar el tamaño del título del eje y
                                  legend.position = "top",  # Mover la leyenda hacia arriba
                                  plot.title = element_text(hjust = 0.5)  # Centralizar el título del gráfico
                                ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = unique(combined_df_ganancias$envios))



tabla_kable_ganancias_sin_input <- kable(tabla_ganancias_sin_input, "html") %>%
  kable_styling()  %>% add_header_above(c("Ganancias sin Inputar - Lightgbm" = 3), escape = FALSE)
print(tabla_kable_ganancias_sin_input)

tabla_kable_ganancias_sin_input_roto <- kable(tabla_ganancias_sin_input_roto, "html") %>%
  kable_styling()  %>% add_header_above(c("Ganancias sin Inputar - Roto" = 3), escape = FALSE)
print(tabla_kable_ganancias_sin_input_roto)

tabla_kable_ganancias_media <- kable(tabla_ganancias_media, "html") %>%
  kable_styling()  %>% add_header_above(c("Ganancias con Media 3 meses Inputada" = 3), escape = FALSE)
print(tabla_kable_ganancias_media)

tabla_kable_ganancias_mediana <- kable(tabla_ganancias_mediana, "html") %>%
  kable_styling()  %>% add_header_above(c("Ganancias con Mediana 3 meses Inputada" = 3), escape = FALSE)
print(tabla_kable_ganancias_mediana)

tabla_kable_ganancias_media_total <- kable(tabla_ganancias_media_total, "html") %>%
  kable_styling()  %>% add_header_above(c("Ganancias con Media total Inputada" = 3), escape = FALSE)
print(tabla_kable_ganancias_media_total)

tabla_kable_ganancias_mediana_total <- kable(tabla_ganancias_mediana_total, "html") %>%
  kable_styling()  %>% add_header_above(c("Ganancias con Mediana total Inputada" = 3), escape = FALSE)
print(tabla_kable_ganancias_mediana_total)















#########
df_gan_sem_si <- aggregate(ganancia ~ semilla, data = df_ganancias_sin_input, FUN = mean)
df_gan_imedia <- aggregate(ganancia ~ semilla, data = df_ganancias_input_media, FUN = mean)
df_gan_imediana <- aggregate(ganancia ~ semilla, data = df_ganancias_input_mediana, FUN = mean)

wilcox.test(df_ganancias_sin_input$ganancia, df_ganancias_input_media$ganancia)

wilcox.test(df_ganancias_sin_input$ganancia, df_ganancias_input_mediana$ganancia)

wilcox.test(df_ganancias_sin_input$ganancia, df_ganancias_sin_input_roto$ganancia)

wilcox.test(df_ganancias_input_media$ganancia, df_ganancias_sin_input_roto$ganancia)
