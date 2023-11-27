# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
require("randomForest")
require("extraTrees")

# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
if (grepl("windows", tolower(Sys.info()["sysname"]))) {
  path <- "C:/Users/maico/Documents/Mestrado/dmeyf2023/kaggle3/"
} else if (grepl("darwin", tolower(Sys.info()["sysname"]))) {
  path <- "/Users/maiconfialho/Documents/Mestrado/2023-2/dmeyf2023/kaggle3/"
}

PARAM <- list()
PARAM$experimento <- "KA8240_competencia_03_lgbm_param_fix"

PARAM$input$dataset <- "./datos/competencia_03_lightgbm_fe_lag3.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(201906, 201907, 201908, 201909, 201910, 201911, 201912, 202001, 202012, 202101, 202102, 202103, 202106, 202107)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo

semillas <- replicate(17, paste(sample(0:9, 6, replace = TRUE), collapse = ""))

# Semeador fixo para reprodução
set.seed(123)

# Melhores hiperparâmetros para LightGBM
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt",
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE,
  verbosity = -100,
  max_depth = -1L,
  min_gain_to_split = 0.0,
  min_sum_hessian_in_leaf = 0.001,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  max_bin = 31L,
  bagging_fraction = 1.0,
  pos_bagging_fraction = 1.0,
  neg_bagging_fraction = 1.0,
  is_unbalance = FALSE,
  scale_pos_weight = 1.0,
  drop_rate = 0.1,
  max_drop = 50,
  skip_drop = 0.5,
  extra_trees = FALSE, # Magic Sauce
  seed = 528881
)

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

# os campos que serão utilizados
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

# estabeleço onde treino
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

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

# Treinamento de modelos
for (semilla in semillas) {
  PARAM$finalmodel$semilla <- semilla
  
  # Treinamento do modelo LightGBM
  param_completo <- c(PARAM$finalmodel$lgb_basicos, PARAM$finalmodel$optim)
  modelo_lgb <- lgb.train(data = dtrain, param = param_completo)
  
  # Adiciona o modelo à lista
  lista_modelos[[paste0("lgb_", semilla)]] <- modelo_lgb
  
  # Treinamento do modelo Random Forest
  modelo_rf <- randomForest(
    x = dataset[train == 1L, campos_buenos, with = FALSE],
    y = dataset[train == 1L, clase01],
    ntree = 100, # Ajuste o número de árvores conforme necessário
    seed = as.numeric(semilla)
  )
  
  # Adiciona o modelo à lista
  lista_modelos[[paste0("rf_", semilla)]] <- modelo_rf
  
  # Treinamento do modelo Extra Trees
  modelo_et <- extraTrees(
    x = dataset[train == 1L, campos_buenos, with = FALSE],
    y = dataset[train == 1L, clase01],
    ntree = 100, # Ajuste o número de árvores conforme necessário
    seed = as.numeric(semilla)
  )
  
  # Adiciona o modelo à lista
  lista_modelos[[paste0("et_", semilla)]] <- modelo_et
}

# Ensemble (bagging)
cortes <- seq(8000, 15000, by = 500)

for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  
  # Bagging: realiza a média das previsões de vários modelos
  for (semilla in semillas) {
    # Obtém o tipo de modelo e a semente
    tipo_modelo <- substr(semilla, 1, 2)
    
    # Seleciona o modelo correto da lista
    if (tipo_modelo == "lgb") {
      modelo_bagging <- lista_modelos[[paste0("lgb_", semilla)]]
    } else if (tipo_modelo == "rf") {
      modelo_bagging <- lista_modelos[[paste0("rf_", semilla)]]
    } else if (tipo_modelo == "et") {
      modelo_bagging <- lista_modelos[[paste0("et_", semilla)]]
    }
    
    # Realiza a previsão
    tb_entrega[, prob := tb_entrega$prob + 
                 predict(modelo_bagging, data.matrix(dapply[, campos_buenos, with = FALSE]))]
  }
  
  tb_entrega[, prob := tb_entrega$prob / length(semillas)]  # Média das previsões
  
  # Atribui 1 aos melhores envios
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]
  
  # Salva arquivo de envio
  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ",")
}

cat("\n\nA geração dos arquivos para o Kaggle foi concluída\n")
