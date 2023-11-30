# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")

# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
if (grepl("windows", tolower(Sys.info()["sysname"]))) {
  path <- "C:/Users/maico/Documents/Mestrado/dmeyf2023/kaggle3/"
} else if (grepl("darwin", tolower(Sys.info()["sysname"]))) {
  path <- "/Users/maiconfialho/Documents/Mestrado/2023-2/dmeyf2023/kaggle3/"
}

PARAM <- list()
PARAM$experimento <- "KA8240_competencia_03_lgbm"

PARAM$input$dataset <- "./datos/competencia_03_fe_lightgbm_lag3.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(201902, 201903, 201904, 201905, 201906, 201907, 201908, 
                          201909, 201910, 201911, 201912, 202001, 202002, 202003, 
                          202101, 202102, 202103, 202104, 202105, 
                          202006, 202007)

PARAM$input$future <- c(202109) # meses donde se aplica el modelo

semillas <- replicate(50, paste(sample(0:9, 6, replace = TRUE), collapse = ""))


#semillas <- c(528881, 583613, 661417, 894407, 915251,
#              173827, 173839, 173867, 547093, 547103,
#             638269, 638303, 638359, 721181, 837451,
#             878173, 910771, 910781, 942659, 942661)

#melhores hp
#Extra_trees = TRUE
#PARAM$finalmodel$optim$num_iterations <- 15
#PARAM$finalmodel$optim$learning_rate <- 0.0556545034638839
#PARAM$finalmodel$optim$feature_fraction <- 0.987208696675516
#PARAM$finalmodel$optim$min_data_in_leaf <- 7502
#PARAM$finalmodel$optim$num_leaves <- 858

#param fix
Extra_trees = TRUE
PARAM$finalmodel$optim$num_iterations <- 26
PARAM$finalmodel$optim$learning_rate <- 0.0669057461273712
PARAM$finalmodel$optim$feature_fraction <- 0.944408946193187
PARAM$finalmodel$optim$min_data_in_leaf <- 33488
PARAM$finalmodel$optim$num_leaves <- 863

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza o programa
setwd(path) # Establezco o Working Directory

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]

# Data Drifting
# por ahora, no hago nada


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso ! corta la bocha
#   https://rdrr.io/cran/data.table/man/shift.html

#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------

# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

lista_resultados <- list()

for (semilla in semillas) {
  
  PARAM$finalmodel$semilla <- semilla
  
# Hiperparametros FIJOS de  lightgbm
  PARAM$finalmodel$lgb_basicos <- list(
    boosting = "gbdt", # puede ir dart, ni pruebe random_forest
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE, # para reducir warnings
    verbosity = -100,
    max_depth = -1L, # -1 significa no limitar, por ahora lo dejo fijo
    min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
    min_sum_hessian_in_leaf = 0.001, # min_sum_hessian_in_leaf >= 0.0
    lambda_l1 = 0.0, # lambda_l1 >= 0.0
    lambda_l2 = 0.0, # lambda_l2 >= 0.0
    max_bin = 31L, # lo debo dejar fijo, no participa de la BO
    
    bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
    pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
    neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
    is_unbalance = FALSE, #
    scale_pos_weight = 1.0, # scale_pos_weight > 0.0
    
    drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
    max_drop = 50, # <=0 means no limit
    skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
    
    extra_trees = Extra_trees, # Magic Sauce
    
    seed = PARAM$finalmodel$semilla
  )
  
  param_completo <- c(PARAM$finalmodel$lgb_basicos, PARAM$finalmodel$optim)
  modelo <- lgb.train(data = dtrain, param = param_completo)
  archivo_importancia <- paste0("impo_", PARAM$finalmodel$semilla, ".txt")
  
  #--------------------------------------
  # ahora imprimo la importancia de variables
  tb_importancia <- as.data.table(lgb.importance(modelo))
  
  # aplico el modelo a los datos sin clase
  dapply <- dataset[foto_mes == PARAM$input$future]
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # genero el dataframe de entrega
  df_entrega <- data.table(
    numero_de_cliente = dapply$numero_de_cliente,
    predicciones = prediccion
  )
  
  # Adiciono el dataframe a la lista de resultados
  lista_resultados[[as.character(semilla)]] <- df_entrega
}

cortes <- seq(8000, 15000, by = 500)

for (envios in cortes) {
  df_final <- data.table(numero_de_cliente = integer())
  
  for (i in seq_along(semillas)) {
    semilla <- semillas[i]
    semilla_pred <- lista_resultados[[as.character(semilla)]][, .(numero_de_cliente, predicciones)]
    setkey(semilla_pred, numero_de_cliente)
    
    col_name <- paste0("pred_", i)
    setnames(semilla_pred, "predicciones", col_name)
    
    df_final <- df_final[semilla_pred, on = "numero_de_cliente"]
  }
  
  df_final[, Predicted := rowMeans(.SD), .SDcols = patterns("^pred_")]
  
  df_final[, Predicted := ifelse(seq_len(.N) <= envios, 1L, 0L)]
  
  lista_resultados[[as.character(envios)]] <- copy(df_final)
  
  write.csv(
    df_final[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    row.names = FALSE
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
