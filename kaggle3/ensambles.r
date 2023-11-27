# Clean memory
rm(list = ls())
gc()

require("data.table")
require("lightgbm")
require("xgboost")
require("randomForest")
require("Metrics")

# Define the parameters
if (grepl("windows", tolower(Sys.info()["sysname"]))) {
  path <- "C:/Users/maico/Documents/Mestrado/dmeyf2023/kaggle3/"
} else if (grepl("darwin", tolower(Sys.info()["sysname"]))) {
  path <- "/Users/maiconfialho/Documents/Mestrado/2023-2/dmeyf2023/kaggle3/"
}

PARAM <- list()
PARAM$experimento <- "KA8240_competencia_03_ensemble"
PARAM$input$dataset <- "./datos/competencia_03_lightgbm_fe_lag3.csv.gz"
PARAM$input$training <- c(201906, 201907, 201908, 201909, 201910, 201911, 201912, 202001, 202012, 202101, 202102, 202103, 202106, 202107)
PARAM$input$future <- c(202109)

semillas <- c(528881, 583613, 661417, 894407, 915251,
              173827, 173839, 173867, 547093, 547103,
              638269, 638303, 638359, 721181, 837451,
              878173, 910771, 910781, 942659, 942661)

setwd(path)

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

setwd(paste0("./exp/", PARAM$experimento, "/"))

dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

# Function to train LightGBM model
train_lightgbm <- function(dtrain, PARAM) {
  param_completo <- c(
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
    extra_trees = TRUE,
    seed = PARAM$finalmodel$semilla,
    PARAM$finalmodel$optim
  )
  modelo <- lgb.train(data = dtrain, param = param_completo)
  return(modelo)
}

# Function to train XGBoost model
train_xgboost <- function(dtrain, PARAM) {
  param_completo <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = 0.3,
    max_depth = 6,
    subsample = 1,
    colsample_bytree = 1,
    min_child_weight = 1,
    gamma = 0,
    seed = PARAM$finalmodel$semilla
  )
  
  # Extract data and label from the lgb.Dataset
  x_data <- data.matrix(dtrain$data)
  y_label <- dtrain$label
  
  # Ensure data is of type 'double' to avoid issues with xgb.DMatrix
  x_data <- as.double(x_data)
  
  # Create XGBoost DMatrix
  dmatrix <- xgb.DMatrix(data = x_data, label = y_label, missing=np.nan)
  
  # Train XGBoost model
  modelo <- xgboost(data = dmatrix, params = param_completo, nrounds = PARAM$finalmodel$optim$num_iterations)
  
  return(modelo)
}


# List to store models
lista_modelos <- list()

# Train LightGBM model
lgb_model <- train_lightgbm(dtrain, PARAM)
lista_modelos[["lgb_model"]] <- lgb_model

# Hyperparameter search for XGBoost
xgb_param_grid <- list(
  eta = c(0.1, 0.01),
  max_depth = c(3, 6, 9),
  subsample = c(0.8, 1),
  colsample_bytree = c(0.8, 1),
  min_child_weight = c(1, 5, 10),
  gamma = c(0, 0.1, 0.2)
)

best_xgb_model <- NULL
best_xgb_logloss <- Inf

for (eta in xgb_param_grid$eta) {
  for (max_depth in xgb_param_grid$max_depth) {
    for (subsample in xgb_param_grid$subsample) {
      for (colsample_bytree in xgb_param_grid$colsample_bytree) {
        for (min_child_weight in xgb_param_grid$min_child_weight) {
          for (gamma in xgb_param_grid$gamma) {
            PARAM$finalmodel$optim <- list(
              num_iterations = 20,
              learning_rate = eta,
              num_leaves = max_depth,
              feature_fraction = colsample_bytree,
              min_data_in_leaf = min_child_weight,
              max_depth = max_depth,
              bagging_fraction = subsample,
              gamma = gamma
            )

            xgb_model <- train_xgboost(dtrain, PARAM)
            pred_probs <- predict(xgb_model, dtrain$data)
            logloss <- logLoss(dtrain$label, pred_probs)

            if (logloss < best_xgb_logloss) {
              best_xgb_logloss <- logloss
              best_xgb_model <- xgb_model
            }
          }
        }
      }
    }
  }
}

# Use the best XGBoost model
lista_modelos[["best_xgb_model"]] <- best_xgb_model

# Hyperparameter search for Random Forest
rf_param_grid <- list(
  ntree = c(50, 100, 150),
  mtry = c(sqrt(ncol(dtrain$data)), ncol(dtrain$data) / 2),
  nodesize = c(10, 20, 30)
)

best_rf_model <- NULL
best_rf_logloss <- Inf

for (ntree in rf_param_grid$ntree) {
  for (mtry in rf_param_grid$mtry) {
    for (nodesize in rf_param_grid$nodesize) {
      PARAM$finalmodel$optim <- list(
        num_iterations = ntree,
        min_data_in_leaf = nodesize
      )

      rf_model <- train_random_forest(dtrain, PARAM)
      pred_probs <- predict(rf_model, dtrain$data)
      logloss <- logLoss(dtrain$label, pred_probs)

      if (logloss < best_rf_logloss) {
        best_rf_logloss <- logloss
        best_rf_model <- rf_model
      }
    }
  }
}

# Use the best Random Forest model
lista_modelos[["best_rf_model"]] <- best_rf_model


# Agora, realizo o bagging
cortes <- seq(8000, 15000, by = 500)

for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  
  # Bagging: realizo a média das previsões de vários modelos
  for (semilla in semillas) {
    modelo_bagging <- lista_modelos[[as.character(semilla)]]
    tb_entrega[, prob := tb_entrega$prob + predict(modelo_bagging, data.matrix(dapply[, campos_buenos, with = FALSE]))]
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
