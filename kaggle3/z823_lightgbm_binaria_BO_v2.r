# Este script está pensado para correr en Google Cloud
#   8 vCPU
# 128 GB memoria RAM

# se entrena con clase_binaria2  POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm,

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("lightgbm")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
    traceback(20)
    options(error = NULL)
    stop("exiting after script error")
})

# Defino una lista de semillas
semillas <- c(100005, 200005, 300005)  # Puedes agregar más semillas según sea necesario

for (i in seq_along(semillas)) {
    # Defino los parámetros de la corrida en una lista, la variable global PARAM
    PARAM <- list()
    
    PARAM$experimento <- paste0("HT8230_", semillas[i])
    
    PARAM$input$dataset <- "./datos/competencia_03_fe_lag3.csv.gz"
    PARAM$input$testing <- c(202105)
    PARAM$input$validation <- c(202104)
    PARAM$input$training <- c(201906, 201907, 201908, 201909, 201910, 201911, 201912, 202001, 202012, 202101, 202102, 202103)
    
    PARAM$trainingstrategy$undersampling <- 1.0
    PARAM$trainingstrategy$semilla_azar <- semillas[i]
    
    PARAM$hyperparametertuning$POS_ganancia <- 273000
    PARAM$hyperparametertuning$NEG_ganancia <- -7000
    
    PARAM$lgb_semilla <- semillas[i]
    
    # Resto del código aquí
    
    # Cambia la carpeta de salida para cada semilla
    dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
    setwd(paste0("./exp/", PARAM$experimento, "/"))
    
    # Resto del código aquí
    
    # Hiperparámetros FIJOS de lightgbm
    PARAM$lgb_basicos <- list(
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
        num_iterations = 20,
        bagging_fraction = 1.0,
        pos_bagging_fraction = 1.0,
        neg_bagging_fraction = 1.0,
        is_unbalance = FALSE,
        scale_pos_weight = 1.0,
        drop_rate = 0.1,
        max_drop = 50,
        skip_drop = 0.5,
        extra_trees = FALSE,
        seed = PARAM$lgb_semilla
    )
    # Aquí se cargan los hiperparámetros que se optimizan
    # en la Bayesian Optimization
    PARAM$bo_lgb <- makeParamSet(
        #makeNumericParam("learning_rate", lower = 0.02, upper = 0.3),
        #makeNumericParam("feature_fraction", lower = 0.01, upper = 1.0),
        #makeIntegerParam("num_leaves", lower = 8L, upper = 1024L),
        #makeIntegerParam("min_data_in_leaf", lower = 100L, upper = 50000L)
        makeNumericParam("learning_rate", 1),
        makeNumericParam("feature_fraction", 0.4),
        makeIntegerParam("num_leaves", 40),
        makeIntegerParam("min_data_in_leaf", 5000)
    )
    
    # Si usted es ambicioso, y tiene paciencia, podria subir este valor a 100
    PARAM$bo_iteraciones <- 50 # iteraciones de la Optimizacion Bayesiana
    
    #------------------------------------------------------------------------------
    # graba a un archivo los componentes de lista
    # para el primer registro, escribe antes los titulos

    loguear <- function(
        reg, arch = NA, folder = "./exp/",
        ext = ".txt", verbose = TRUE) {
    archivo <- arch
    if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

    if (!file.exists(archivo)) # Escribo los titulos
        {
        linea <- paste0(
            "fecha\t",
            paste(list.names(reg), collapse = "\t"), "\n"
        )

        cat(linea, file = archivo)
        }

    linea <- paste0(
        format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
        gsub(", ", "\t", toString(reg)), "\n"
    )

    cat(linea, file = archivo, append = TRUE) # grabo al archivo

    if (verbose) cat(linea) # imprimo por pantalla
    }
    #------------------------------------------------------------------------------
    GLOBAL_arbol <- 0L
    GLOBAL_gan_max <- -Inf
    vcant_optima <- c()

    fganancia_lgbm_meseta <- function(probs, datos) {
    vlabels <- get_field(datos, "label")
    vpesos <- get_field(datos, "weight")


    GLOBAL_arbol <<- GLOBAL_arbol + 1
    tbl <- as.data.table(list(
        "prob" = probs,
        "gan" = ifelse(vlabels == 1 & vpesos > 1,
        PARAM$hyperparametertuning$POS_ganancia,
        PARAM$hyperparametertuning$NEG_ganancia  )
    ))

    setorder(tbl, -prob)
    tbl[, posicion := .I]
    tbl[, gan_acum := cumsum(gan)]

    tbl[, gan_suavizada :=
        frollmean(
        x = gan_acum, n = 2001, align = "center",
        na.rm = TRUE, hasNA = TRUE
        )]

    gan <- tbl[, max(gan_suavizada, na.rm = TRUE)]


    pos <- which.max(tbl[, gan_suavizada])
    vcant_optima <<- c(vcant_optima, pos)

    if (GLOBAL_arbol %% 10 == 0) {
        if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan

        cat("\r")
        cat(
        "Validate ", GLOBAL_iteracion, " ", " ",
        GLOBAL_arbol, "  ", gan, "   ", GLOBAL_gan_max, "   "
        )
    }


    return(list(
        "name" = "ganancia",
        "value" = gan,
        "higher_better" = TRUE
    ))
    }
    #------------------------------------------------------------------------------

    EstimarGanancia_lightgbm <- function(x) {
    gc()
    GLOBAL_iteracion <<- GLOBAL_iteracion + 1L

    # hago la union de los parametros basicos y los moviles que vienen en x
    param_completo <- c(PARAM$lgb_basicos, x)

    param_completo$early_stopping_rounds <-
        as.integer(400 + 4 / param_completo$learning_rate)

    GLOBAL_arbol <<- 0L
    GLOBAL_gan_max <<- -Inf
    vcant_optima <<- c()
    set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
    modelo_train <- lgb.train(
        data = dtrain,
        valids = list(valid = dvalidate),
        eval = fganancia_lgbm_meseta,
        param = param_completo,
        verbose = -100
    )

    cat("\n")

    cant_corte <- vcant_optima[modelo_train$best_iter]

    # aplico el modelo a testing y calculo la ganancia
    prediccion <- predict(
        modelo_train,
        data.matrix(dataset_test[, campos_buenos, with = FALSE])
    )

    tbl <- copy(dataset_test[, list("gan" = ifelse(clase_ternaria == "BAJA+2",
        PARAM$hyperparametertuning$POS_ganancia, 
        PARAM$hyperparametertuning$NEG_ganancia))])

    tbl[, prob := prediccion]
    setorder(tbl, -prob)
    tbl[, gan_acum := cumsum(gan)]
    tbl[, gan_suavizada := frollmean(
        x = gan_acum, n = 2001,
        align = "center", na.rm = TRUE, hasNA = TRUE
    )]


    ganancia_test <- tbl[, max(gan_suavizada, na.rm = TRUE)]

    cantidad_test_normalizada <- which.max(tbl[, gan_suavizada])

    rm(tbl)
    gc()

    ganancia_test_normalizada <- ganancia_test


    # voy grabando las mejores column importance
    if (ganancia_test_normalizada > GLOBAL_gananciamax) {
        GLOBAL_gananciamax <<- ganancia_test_normalizada
        tb_importancia <- as.data.table(lgb.importance(modelo_train))

        fwrite(tb_importancia,
        file = paste0("impo_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
        sep = "\t"
        )

        rm(tb_importancia)
    }


    # logueo final
    ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
    xx <- c(ds, copy(param_completo))

    xx$early_stopping_rounds <- NULL
    xx$num_iterations <- modelo_train$best_iter
    xx$estimulos <- cantidad_test_normalizada
    xx$ganancia <- ganancia_test_normalizada
    xx$iteracion_bayesiana <- GLOBAL_iteracion

    loguear(xx, arch = "BO_log.txt")

    set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
    return(ganancia_test_normalizada)
    }
    #------------------------------------------------------------------------------
    #------------------------------------------------------------------------------
    # Aqui empieza el programa

    # Aqui se debe poner la carpeta de la computadora local
    #setwd("~/buckets/b1/") # Establezco el Working Directory
    setwd("C:/Users/maico/Documents/Mestrado/dmeyf2023/kaggle3/") # Establezco el Working Directory

    # cargo el dataset donde voy a entrenar el modelo
    dataset <- fread(PARAM$input$dataset)


    # creo la carpeta donde va el experimento
    dir.create("./exp/", showWarnings = FALSE)
    dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

    # Establezco el Working Directory DEL EXPERIMENTO
    setwd(paste0("./exp/", PARAM$experimento, "/"))

    # en estos archivos quedan los resultados
    kbayesiana <- paste0(PARAM$experimento, ".RDATA")
    klog <- paste0(PARAM$experimento, ".txt")



    # Catastrophe Analysis  -------------------------------------------------------
    # deben ir cosas de este estilo
    #   dataset[foto_mes == 202006, active_quarter := NA]

    # Data Drifting
    # por ahora, no hago nada


    # Feature Engineering Historico  ----------------------------------------------
    #   aqui deben calcularse los  lags y  lag_delta
    #   Sin lags no hay paraiso !  corta la bocha


    # ahora SI comienza la optimizacion Bayesiana

    GLOBAL_iteracion <- 0 # inicializo la variable global
    GLOBAL_gananciamax <- -1 # inicializo la variable global

    # si ya existe el archivo log, traigo hasta donde llegue
    if (file.exists(klog)) {
    tabla_log <- fread(klog)
    GLOBAL_iteracion <- nrow(tabla_log)
    GLOBAL_gananciamax <- tabla_log[, max(ganancia)]
    }



    # paso la clase a binaria que tome valores {0,1}  enteros
    dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]


    # los campos que se van a utilizar
    campos_buenos <- setdiff(
    colnames(dataset),
    c("clase_ternaria", "clase01", "azar", "training")
    )
    
    # Defino los datos que forman parte del training
    # Aquí se hace el undersampling de los CONTINUA
    set.seed(PARAM$trainingstrategy$semilla_azar)
    dataset[, azar := runif(nrow(dataset))]
    dataset[, training := 0L]
    dataset[
        foto_mes %in% PARAM$input$training &
        (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
        training := 1L
    ]
    
    # Dejo los datos en el formato que necesita LightGBM
    dtrain <- lgb.Dataset(
        data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
        label = dataset[training == 1L, clase01],
        weight = dataset[training == 1L, 
        ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
            ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
        free_raw_data = FALSE
    )
    
    # defino los datos que forman parte de validation
    #  no hay undersampling
    dataset[, validation := 0L]
    dataset[ foto_mes %in% PARAM$input$validation,  validation := 1L]

    dvalidate <- lgb.Dataset(
    data = data.matrix(dataset[validation == 1L, campos_buenos, with = FALSE]),
    label = dataset[validation == 1L, clase01],
    weight = dataset[validation == 1L, 
        ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
        ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
    free_raw_data = FALSE
    )


    # defino los datos de testing
    dataset[, testing := 0L]
    dataset[ foto_mes %in% PARAM$input$testing,  testing := 1L]


    dataset_test <- dataset[testing == 1, ]

    # libero espacio
    rm(dataset)
    gc()

    # Aquí comienza la configuración de la Bayesian Optimization
    funcion_optimizar <- EstimarGanancia_lightgbm # la función que voy a maximizar

    # Configuro la búsqueda bayesiana,  los hiperparámetros que se van a optimizar
    obj.fun <- makeSingleObjectiveFunction(
        fn = funcion_optimizar, # la función que voy a maximizar
        minimize = FALSE, # estoy Maximizando la ganancia
        noisy = TRUE,
        par.set = PARAM$bo_lgb, # definido al comienzo del programa
        has.simple.signature = FALSE # paso los parámetros en una lista
    )

    # Cada 600 segundos guardo el resultado intermedio
    ctrl <- makeMBOControl(
        save.on.disk.at.time = 600, # se graba cada 600 segundos
        save.file.path = kbayesiana
    ) # se graba cada 600 segundos

    # Indico la cantidad de iteraciones que va a tener la Bayesian Optimization
    ctrl <- setMBOControlTermination(
        ctrl,
        iters = PARAM$bo_iteraciones
    ) # cantidad de iteraciones

    # Defino el método estándar para la creación de los puntos iniciales,
    # los "No Inteligentes"
    ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

    # establezco la funcion que busca el maximo
    surr.km <- makeLearner(
    "regr.km",
    predict.type = "se",
    covtype = "matern3_2",
    control = list(trace = TRUE)
    )

    # Inicio la optimización bayesiana
    if (!file.exists(kbayesiana)) {
        run <- mbo(obj.fun, learner = surr.km, control = ctrl)
    } else {
        run <- mboContinue(kbayesiana) # retomo en caso que ya exista
    }

    cat("\n\nLa optimización Bayesiana ha terminado\n")
}
