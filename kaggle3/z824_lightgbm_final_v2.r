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
PARAM$experimento <- "KA8240_competencia_03_40s"

PARAM$input$dataset <- "./datos/competencia_03_fe_lag3.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(201906, 201907, 201908, 201909, 201910, 201911, 201912, 202001, 202012, 202101, 202102, 202103)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo

semillas <- replicate(20, paste(sample(0:9, 6, replace = TRUE), collapse = ""))


#semillas <- c(528881, 583613, 661417, 894407, 915251,
#              173827, 173839, 173867, 547093, 547103,
#              638269, 638303, 638359, 721181, 837451,
#              878173, 910771, 910781, 942659, 942661)

# hiperparâmetros intencionalmente NÃO otimizados
PARAM$finalmodel$optim$num_iterations <- 18
PARAM$finalmodel$optim$learning_rate <- 0.0516060767778788
PARAM$finalmodel$optim$feature_fraction <- 0.464346570655767
PARAM$finalmodel$optim$min_data_in_leaf <- 1043
PARAM$finalmodel$optim$num_leaves <- 469

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza o programa
setwd(path) # Establezco o Working Directory

# cargo o dataset onde vou treinar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# Catastrophe Analysis -------------------------------------------------------
# devem ir coisas desse estilo
# dataset[foto_mes == 202006, active_quarter := NA]

# Data Drifting
# por agora, não faço nada

# Feature Engineering Historico ----------------------------------------------
# aqui devem calcular os lags e lag_delta
# Sem lags não há paraíso! corta a bocha
# https://rdrr.io/cran/data.table/man/shift.html

#--------------------------------------

# passo a classe a binária que toma valores {0,1} inteiros
# set trabalha com a classe POS = {BAJA+1, BAJA+2}
# esta estratégia é MUITO importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# os campos que serão utilizados
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------

# estabeleço onde treino
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# crio as pastas onde vão os resultados
# crio a pasta onde vai o experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Estabeleço o Working Directory DO EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# deixo os dados no formato que o LightGBM precisa
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

# Lista para armazenar os modelos
lista_modelos <- list()

for (semilla in semillas) {
  PARAM$finalmodel$semilla <- semilla
  
  # Hiperparâmetros FIXOS de lightgbm
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
    
    extra_trees = TRUE, # Magic Sauce
    
    seed = PARAM$finalmodel$semilla
  )
  
  
  # gero o modelo
  param_completo <- c(PARAM$finalmodel$lgb_basicos, PARAM$finalmodel$optim)
  modelo <- lgb.train(data = dtrain, param = param_completo)
  archivo_importancia <- paste0("impo_", PARAM$finalmodel$semilla, ".txt")
  
  #--------------------------------------
  # ahora imprimo la importancia de variables
  tb_importancia <- as.data.table(lgb.importance(modelo))
  #archivo_importancia <- "impo_se.txt"
  
  #fwrite(tb_importancia,
  #       file = archivo_importancia,
  #       sep = "\t"
  #)
  
  #--------------------------------------
  
  
  # aplico el modelo a los datos sin clase
  dapply <- dataset[foto_mes == PARAM$input$future]
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # genero la tabla de entrega
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
  tb_entrega[, prob := prediccion]
  
  # grabo las probabilidad del modelo
  #fwrite(tb_entrega,
  #       file = paste0("prediccion_", PARAM$finalmodel$semilla, ".txt"),
  #       sep = "\t"
  #)
  
  # ordeno por probabilidad descendente
  setorder(tb_entrega, -prob)
  
  # Adiciono o modelo à lista
  lista_modelos[[as.character(semilla)]] <- modelo
}

# Agora, realizo o bagging
cortes <- seq(8000, 15000, by = 500)

for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  
  # Bagging: realizo a média das previsões de vários modelos
  for (semilla in semillas) {
    modelo_bagging <- lista_modelos[[as.character(semilla)]]
    #print(modelo_bagging)
    tb_entrega[, prob := tb_entrega$prob + 
                 predict(modelo_bagging, data.matrix(dapply[, campos_buenos, with = FALSE]))]
  }        
  
  tb_entrega[, prob := tb_entrega$prob / length(semillas)]  # Média das previsões
  
  # Atribuir 1 aos melhores envios
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]
  
  # Salvar arquivo de envio
  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ",")
}

cat("\n\nA geração dos arquivos para o Kaggle foi concluída\n")
