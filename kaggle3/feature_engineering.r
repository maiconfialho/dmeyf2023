rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
library(dplyr)

#setwd("/Users/maiconfialho/Documents/Mestrado/2023-2/dmeyf2023/") # Establezco el Directorio de Trabajo
setwd("C:/Users/maico/Documents/Mestrado/dmeyf2023/kaggle3/") # Establezco el Directorio de Trabajo
#dataset <- fread("./datos/dataset_inputado_media_por_cliente_3_meses.csv.gz")
dataset <- fread("./datos/dataset_inputado_mediana_por_cliente_3_meses.csv.gz")

dataset <- dataset[order(numero_de_cliente, foto_mes)]

library(dplyr)


dataset <- dataset %>%
  mutate_all(funs(lag1 = lag(.), lag2 = lag(., 2), lag3 = lag(., 3)))

# Total de deudas
colunas_deudas <- c("mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios", "Visa_msaldototal", "Master_msaldototal")
dataset[, deudas := rowSums(.SD, na.rm = TRUE), .SDcols = colunas_deudas]

# Total de activos
colunas_activos <- c("mplazo_fijo_dolares", "mplazo_fijo_pesos", "minversion1_pesos", "minversion1_dolares", "minversion2", "mcuentas_saldo")
dataset[, activos := rowSums(.SD, na.rm = TRUE), .SDcols = colunas_activos]

# Saldo líquido, activo - deuda
dataset$balance <- dataset$activos - dataset$deudas

# Invierte dinero en el banco
dataset[, invierte := (cinversion1 != 0 | cinversion2 != 0)]

# Invierte más que el mes pasado
dataset[, ultimo_minversion1_pesos := shift(minversion1_pesos, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, ultimo_minversion1_dolares := shift(minversion1_dolares, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, ultimo_minversion2 := shift(minversion2, n = -1, type = "lead", fill = 0), by = numero_de_cliente]

dataset[, ultimo_num_inversion1 := shift(cinversion1, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, ultimo_num_inversion2 := shift(cinversion2, n = -1, type = "lead", fill = 0), by = numero_de_cliente]

# Verifica si los valores actuales son menores que los últimos valores
dataset[, disminuye_val_inversion := (minversion1_pesos < ultimo_minversion1_pesos |
                                        minversion1_dolares < ultimo_minversion1_dolares |
                                        minversion2 < ultimo_minversion2)]

dataset[, disminuye_num_inversion := (cinversion1 < ultimo_num_inversion1 |
                                        cinversion2 < ultimo_num_inversion2)]

# Qué cuentas tienen saldos
dataset[, cuentacorriente_saldo_pos := ifelse(mcuenta_corriente > 0, 1, 0)]
dataset[, cajaahorro_saldo_pos := ifelse(mcaja_ahorro > 0, 1, 0)]
dataset[, saldo_pos := ifelse(mcaja_ahorro + mcuenta_corriente > 0, 1, 0)]

# Cuánto tiempo la mcuenta_corriente no aumenta el saldo
dataset[, saldo_anterior_corriente := shift(mcuenta_corriente, n = -1, fill = 0), by = numero_de_cliente]
dataset[, aumento_saldo_corriente := mcuenta_corriente > saldo_anterior_corriente]
dataset[, tiempo_sin_aumento_cc := cumsum(!aumento_saldo_corriente), by = numero_de_cliente]
dataset[aumento_saldo_corriente == TRUE, tiempo_sin_aumento := 0, by = numero_de_cliente]

# Cuánto tiempo la mcaja_ahorro no aumenta el saldo
dataset[, saldo_anterior_ahorro := shift(mcaja_ahorro, n = -1, fill = 0), by = numero_de_cliente]
dataset[, aumento_saldo_ahorro := mcaja_ahorro > saldo_anterior_ahorro]
dataset[, tiempo_sin_aumento_ah := cumsum(!aumento_saldo_ahorro), by = numero_de_cliente]
dataset[aumento_saldo_ahorro == TRUE, tiempo_sin_aumento := 0, by = numero_de_cliente]

# Cuánto tiempo la mcuenta_corriente no disminuye el saldo
dataset[, disminuye_saldo_corriente := mcuenta_corriente < saldo_anterior_corriente]
dataset[, tiempo_sin_disminuir_cc := cumsum(!disminuye_saldo_corriente), by = numero_de_cliente]
dataset[disminuye_saldo_corriente == TRUE, tiempo_sin_disminuir := 0, by = numero_de_cliente]

# Cuánto tiempo la mcaja_ahorro no disminuye el saldo
dataset[, disminuye_saldo_ahorro := mcaja_ahorro < saldo_anterior_ahorro]
dataset[, tiempo_sin_disminuir_ah := cumsum(!disminuye_saldo_ahorro), by = numero_de_cliente]
dataset[disminuye_saldo_ahorro == TRUE, tiempo_sin_disminuir := 0, by = numero_de_cliente]

# Cantidad total de tarjetas de crédito
dataset[, total_tarjetas := ctarjeta_visa + ctarjeta_master]

# Disminuyó la cantidad de tarjetas de crédito
dataset[, total_tarjetas_ult_mes := shift(total_tarjetas, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, disminuyo_tarjetas := (total_tarjetas < total_tarjetas_ult_mes)]

# Usa débito
dataset[, usa_tarjeta_debito := ifelse(dataset$ctarjeta_debito_transacciones > 0, 1, 0)]

# Disminuyó el uso de débito
dataset[, uso_tar_debito_ult_mes := shift(ctarjeta_debito_transacciones, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, disminuyo_uso_tar_debito := (ctarjeta_debito_transacciones < uso_tar_debito_ult_mes)]

# Tiene tarjeta Visa
dataset[, tiene_visa := ifelse(dataset$ctarjeta_visa > 0, 1, 0)]

# Usa tarjeta Visa
dataset[, usa_tarjeta_visa := ifelse(dataset$ctarjeta_visa_transacciones > 0, 1, 0)]

# Disminuyó el uso de tarjeta Visa
dataset[, uso_tar_visa_ult_mes := shift(ctarjeta_visa_transacciones, n = -1, type = "lead", fill = 0), by = numero_de_cliente]

# Tiene tarjeta Master
dataset[, tiene_master := ifelse(dataset$ctarjeta_master > 0, 1, 0)]

# Disminuyó el uso de tarjeta Master
dataset[, uso_tar_master_ult_mes := shift(ctarjeta_master_transacciones, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, disminuyo_uso_tar_master := (ctarjeta_master_transacciones < uso_tar_master_ult_mes)]

# Tiene seguro
dataset[, tiene_seguro := cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]

# Disminuyó el uso de seguros
dataset[, uso_seguro_ult_mes := shift(tiene_seguro, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, disminuyo_uso_seguro := (tiene_seguro < uso_seguro_ult_mes)]

# Recibe el salario en el banco
dataset[, recibe_sueldo := ifelse(dataset$cpayroll_trx + dataset$cpayroll2_trx > 0, 1, 0)]

# Dejó de recibir el salario en el banco
dataset[, paro_de_recibir_sueldo := shift(recibe_sueldo, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, paro_de_recibir_sueldo := (recibe_sueldo < paro_de_recibir_sueldo)]

# Cantidad total de débitos automáticos
dataset[, qtd_debito_automatico := ccuenta_debitos_automaticos + ctarjeta_visa_debitos_automaticos + ctarjeta_master_debitos_automaticos]

# Tiene débito automático
dataset[, tiene_debito_automatico := ifelse(qtd_debito_automatico > 0, 1, 0)]

# cantidade de debitos automaticos ultimo mes
dataset[, qtd_debito_automatico_ult_mes := shift(qtd_debito_automatico, n = -1, type = "lead", fill = 0), by = numero_de_cliente]

# Cantidad de débitos automáticos disminuyó
dataset[, disminuyo_debito_automatico := (qtd_debito_automatico < qtd_debito_automatico_ult_mes)]

# Cantidad de débitos automáticos aumentó
dataset[, aumento_debito_automatico := (qtd_debito_automatico > qtd_debito_automatico_ult_mes)]

# Pago mis cuentas
dataset[, pago_mis_cuentas := ifelse(dataset$cpagomiscuentas > 0, 1, 0)]

# Pago mis cuentas ultimo mes
dataset[, pago_mis_cuentas_ult_mes := shift(pago_mis_cuentas, n = -1, type = "lead", fill = 0), by = numero_de_cliente]

# Cantidad de pago mis cuentas disminuyó o aumentó
dataset[, disminuyo_pago_mis_cuentas := (pago_mis_cuentas < pago_mis_cuentas_ult_mes)]
dataset[, aumento_pago_mis_cuentas := (pago_mis_cuentas > pago_mis_cuentas_ult_mes)]

# Pago de servicios
dataset[, pago_servicios := ifelse(dataset$cpagodeservicios > 0, 1, 0)]

# Pago de servicios ultimo mes
dataset[, pago_servicios_ult_mes := shift(pago_servicios, n = -1, type = "lead", fill = 0), by = numero_de_cliente]

# Cantidad de pago de servicios aumentó o disminuyó
dataset[, disminuyo_pago_servicios := (pago_servicios < pago_servicios_ult_mes)]
dataset[, aumento_pago_servicios := (pago_servicios > pago_servicios_ult_mes)]

# Hace transferencias
dataset[, hace_transferencias := ifelse(dataset$mtransferencias_emitidas > 0, 1, 0)]

# transferencias emitidas ultimo mes
dataset[, transferencias_emitidas_ult_mes := shift(mtransferencias_emitidas, n = -1, type = "lead", fill = 0), by = numero_de_cliente]

# Recibe transferencias
dataset[, recibe_transferencias := ifelse(dataset$mtransferencias_recibidas > 0, 1, 0)]

# transferencias recibidas ultimo mes
dataset[, transferencias_recibidas_ult_mes := shift(mtransferencias_recibidas, n = -1, type = "lead", fill = 0), by = numero_de_cliente]

# Balance de transferencias
dataset[, balance_transferencias := mtransferencias_recibidas - mtransferencias_emitidas]


# Cantidad de transferencias emitidas disminuyó o aumentó
dataset[, disminuyo_transferencias_emitidas := (mtransferencias_emitidas < transferencias_emitidas_ult_mes)]
dataset[, aumento_transferencias_emitidas := (mtransferencias_emitidas > transferencias_emitidas_ult_mes)]

# Cantidad de transferencias recibidas disminuyó o aumentó
dataset[, disminuyo_transferencias_recibidas := (mtransferencias_recibidas < transferencias_recibidas_ult_mes)]
dataset[, aumento_transferencias_recibidas := (mtransferencias_recibidas > transferencias_recibidas_ult_mes)]

# Sacó más que depositó
dataset[, saco_mas_que_deposito := (ccajas_extracciones > ccajas_depositos)]

# Depositó menos que el mes pasado
dataset[, deposito_menos_que_ultimo_mes := (ccajas_depositos < shift(ccajas_depositos, n = -1, type = "lead", fill = 0)), by = numero_de_cliente]

# Sacó más que el mes pasado
dataset[, saco_mas_que_ultimo_mes := (ccajas_extracciones > shift(ccajas_extracciones, n = -1, type = "lead", fill = 0)), by = numero_de_cliente]

# Número de saques está por encima de la media de saque de los últimos 3 meses
dataset[, media_3_meses_saque := shift(mean(ccajas_extracciones), n = -3, fill = 0), by = numero_de_cliente]
dataset[, saques_mayores_que_media := ccajas_extracciones > media_3_meses_saque]

# Número de cheques depositados es menor que la media de los cheques depositados de los últimos tres meses
dataset[, media_3_meses_cheques_depositados := shift(mean(ccheques_depositados), n = -3, fill = 0), by = numero_de_cliente]
dataset[, cheques_depositados_menores_que_media := ccheques_depositados < media_3_meses_cheques_depositados]

# Número de cheques emitidos es menor que la media de los cheques emitidos de los últimos tres meses
dataset[, media_3_meses_cheques_emitidos := shift(mean(ccheques_emitidos), n = -3, fill = 0), by = numero_de_cliente]
dataset[, cheques_emitidos_menores_que_media := ccheques_emitidos < media_3_meses_cheques_emitidos]

# Hace más transacciones en otro cajero (matm_other)
dataset[, otros_cajeros := ifelse(dataset$matm < dataset$matm_other, 1, 0)]

# Eliminar variables innecesarias
columnas_para_eliminar <- c("ultimo_minversion1_pesos", "ultimo_minversion1_dolares", "ultimo_minversion2",
                            "ultimo_num_inversion1", "ultimo_num_inversion2",
                            "saldo_anterior_ahorro", "saldo_anterior_corriente", "total_tarjetas_ult_mes",
                            "uso_tar_debito_ult_mes", "uso_tar_visa_ult_mes", "uso_tar_master_ult_mes",
                            "uso_seguro_ult_mes", "paro_de_recibir_sueldo", "media_3_meses_saque",
                            "media_3_meses_cheques_depositados", "media_3_meses_cheques_emitidos",
                            "qtd_debito_automatico_ult_mes", "qtd_debito_automatico_ult_mes",
                            "pago_mis_cuentas_ult_mes", "pago_servicios_ult_mes", 
                            "transferencias_emitidas_ult_mes", "transferencias_recibidas_ult_mes")

# Eliminar las columnas especificadas del conjunto de datos
#dataset <- dataset[, !columnas_para_eliminar, with = FALSE]
#dataset <- dataset[, !(names(dataset) %in% columnas_para_eliminar)]
dataset <- dataset %>% select(-one_of(columnas_para_eliminar))

#infinitos <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
#infinitos_qty <- sum(unlist(infinitos))

#nans <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
#nans_qty <- sum(unlist(nans))

fwrite(dataset,
        "./datos/competencia_03_mediana3_fe_lag3.csv.gz",
        logical01= TRUE,
        sep= "," )
dataset_inputado_media_por_cliente_3_meses