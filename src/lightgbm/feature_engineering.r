require("data.table")

setwd("C:/Users/maico/Documents/Mestrado/dmeyf2023/") # Establezco el Working Directory
dataset <- fread("./datasets/competencia_02.csv.gz")
dataset <- dataset[order(numero_de_cliente, foto_mes)]

#total de deudas
colunas_deudas <- c("mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios", "Visa_msaldototal", "Master_msaldototal")
dataset[, deudas := rowSums(.SD, na.rm = TRUE), .SDcols = colunas_deudas]
#total de activos
colunas_activos <- c("mplazo_fijo_dolares", "mplazo_fijo_pesos", "minversion1_pesos", "minversion1_dolares", "minversion2", "mcuentas_saldo")
dataset[, activos := rowSums(.SD, na.rm = TRUE), .SDcols = colunas_activos]


#saldo liquido, activo - deuda
dataset$balance <- dataset$activos - dataset$deudas

#investe dinheiro no banco
dataset[, invierte := (cinversion1 != 0 | cinversion2 != 0)]

#investimento diminuiu nos ultimos 3 meses?
# Calcule o valor de investimento de 3 meses atrás
# Calcule a média dos últimos 3 meses para cada coluna de investimento
dataset[, ultimo_minversion1_pesos := shift(minversion1_pesos, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, ultimo_minversion1_dolares := shift(minversion1_dolares, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, ultimo_minversion2 := shift(minversion2, n = -1, type = "lead", fill = 0), by = numero_de_cliente]

dataset[, ultimo_num_inversion1 := shift(cinversion1, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, ultimo_num_inversion2 := shift(cinversion2, n = -1, type = "lead", fill = 0), by = numero_de_cliente]


# Verifique se os valores atuais são menores que os últimos valores
dataset[, diminiu_val_investimento := (minversion1_pesos < ultimo_minversion1_pesos |
                               minversion1_dolares < ultimo_minversion1_dolares |
                               minversion2 < ultimo_minversion2)]

dataset[, diminiu_num_investimento := (cinversion1 < ultimo_num_inversion1 |
                               cinversion2 < ultimo_num_inversion2)]

#quais contas tem saldos
dataset[, has_cuentacorriente_saldo_pos := ifelse(mcuenta_corriente > 0, 1, 0)]
dataset[, has_cajaahorro_saldo_pos := ifelse(mcaja_ahorro > 0, 1, 0)]
dataset[, has_saldo_pos := ifelse(mcaja_ahorro + mcuenta_corriente > 0, 1, 0)]

#quanto tempo a mcuenta_corriente nao aumenta o saldo
dataset[, saldo_anterior := shift(mcuenta_corrente, n = -1, fill = 0), by = numero_de_cliente]
dataset[, aumento_saldo_corriente := mcuenta_corrente > saldo_anterior]
dataset[, tempo_sem_aumento := cumsum(!aumento_saldo_corriente), by = numero_de_cliente]
dataset[aumento_saldo_corriente == TRUE, tempo_sem_aumento := 0, by = numero_de_cliente]

#quanto tempo a mcaja_ahorro nao aumenta o saldo
dataset[, saldo_anterior := shift(mcaja_ahorro, n = -1, fill = 0), by = numero_de_cliente]
dataset[, aumento_saldo_ahorro := mcaja_ahorro > saldo_anterior]
dataset[, tempo_sem_aumento := cumsum(!aumento_saldo_ahorro), by = numero_de_cliente]
dataset[aumento_saldo_ahorro == TRUE, tempo_sem_aumento := 0, by = numero_de_cliente]

#quanto tempo a mcuenta_corriente nao diminiuiu o saldo
dataset[, saldo_anterior := shift(mcuenta_corriente, n = -1, fill = 0), by = numero_de_cliente]
dataset[, diminuiu_saldo_corriente := mcuenta_corrente < saldo_anterior]
dataset[, tempo_sem_diminuiu := cumsum(!diminuiu_saldo_corriente), by = numero_de_cliente]
dataset[diminuiu_saldo_corriente == TRUE, tempo_sem_diminuiu := 0, by = numero_de_cliente]

#quanto tempo a mcaja_ahorro nao diminuiu o saldo
dataset[, saldo_anterior := shift(mcaja_ahorro, n = -1, fill = 0), by = numero_de_cliente]
dataset[, diminuiu_saldo_ahorro := mcaja_ahorro < saldo_anterior]
dataset[, tempo_sem_diminuiu := cumsum(!diminuiu_saldo_ahorro), by = numero_de_cliente]
dataset[diminuiu_saldo_ahorro == TRUE, tempo_sem_diminuiu := 0, by = numero_de_cliente]

#quantidade total de cartoes de credito
dataset[, total_cartoes := ctarjeta_visa + ctarjeta_master]
#diminuiu a quantidade de cartoes de credito
dataset[, total_cartoes_ult_mes := shift(total_cartoes, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, diminuiu_cartoes := (total_cartoes < total_cartoes_ult_mes)]

#usa debito
dataset[, usa_tarjeta_debito := ifelse(dataset$ctarjeta_debito_transacciones > 0, 1, 0) ]
#diminuiu uso de debito
dataset[, uso_tar_debito_ult_mes := shift(ctarjeta_debito_transacciones, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, diminiui_uso_tar_debito := (ctarjeta_debito_transacciones < uso_tar_debito_ult_mes)]
#tiene credito visa
dataset[, tiene_visa := ifelse(dataset$ctarjeta_visa > 0, 1, 0) ]
#usa tarnejta visa
dataset[, usa_tarjeta_visa := ifelse(dataset$ctarjeta_visa_transacciones > 0, 1, 0) ]
#diminuiu uso de credito visa
dataset[, uso_tar_visa_ult_mes := shift(ctarjeta_visa_transacciones, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
#tiene master
dataset[, tiene_master := ifelse(dataset$ctarjeta_master > 0, 1, 0)]
#diminuiu uso de credito master
dataset[, uso_tar_master_ult_mes := shift(ctarjeta_master_transacciones, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, diminiui_uso_tar_master := (ctarjeta_master_transacciones < uso_tar_master_ult_mes)]
#tem seguro
dataset[, tiene_seguro := cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]
#diminiu os seguros
dataset[, uso_seguro_ult_mes := shift(tiene_seguro, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, diminiui_uso_seguro := (tiene_seguro < uso_seguro_ult_mes)]
#recebe o salario no banco
dataset[, recibe_sueldo := ifelse(dataset$cpayroll_trx + dataset$cpayroll2_trx  > 0, 1, 0)]
#parou de receber o salario no banco
dataset[, rec_sueld__ult_mes := shift(recibe_sueldo, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, parou_de_receber_sueldo := (recibe_sueldo < rec_sueld__ult_mes)]
#quantidade de debitos automaticos
dataset[, qtd_debito_automatico := ccuenta_debitos_automaticos + tarjeta_visa_debitos_automaticos + ctarjeta_master_debitos_automaticos]
#tem debito automatico
dataset[, tiene_debito_automatico := ifelse(qtd_debito_automatico > 0, 1, 0)]
#quantidade de debitos automaticos diminiuiu?
dataset[, qtd_debito_automatico_ult_mes := shift(qtd_debito_automatico, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, diminiuiu_debito_automatico := (qtd_debito_automatico < qtd_debito_automatico_ult_mes)]
#quantidade de debitos automaticos aumentou?
dataset[, aumentou_debito_automatico := (qtd_debito_automatico > qtd_debito_automatico_ult_mes)]
#pago mi cuentas
dataset[, pago_mis_cuentas := ifelse(dataset$cpagomiscuentas  > 0, 1, 0) ]
#quantidade de pago mis cuentas diminuiu ou aumentou?
dataset[, pago_mis_cuentas_ult_mes := shift(pago_mis_cuentas, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, diminiuiu_pago_mis_cuentas := (pago_mis_cuentas < pago_mis_cuentas_ult_mes)]
dataset[, aumentou_pago_mis_cuentas := (pago_mis_cuentas > pago_mis_cuentas_ult_mes)]
#pago de servicios
dataset[, pago_servicios := ifelse(dataset$cpagodeservicios  > 0, 1, 0)]
#quantidade de pago servicios aumentou ou diminiuiu?
dataset[, pago_servicios_ult_mes := shift(pago_servicios, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, diminiuiu_pago_servicios := (pago_servicios < pago_servicios_ult_mes)]
dataset[, aumentou_pago_servicios := (pago_servicios > pago_servicios_ult_mes)]
#faz transferencias?
dataset[, faz_transferencias := ifelse(dataset$mtransferencias_emitidas> 0, 1, 0)]
#recebe transferencias?
dataset[, recebe_transferencias := ifelse(dataset$mtransferencias_recibidas> 0, 1, 0)]
#balanco de transferencias
dataset[, balance_transferencias := mtransferencias_recibidas - mtransferencias_emitidas]
#quantidade de transferencias emitidas diminiuiu ou aumentou?
dataset[, transferencias_emitidas_ult_mes := shift(mtransferencias_emitidas, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, diminiuiu_transferencias_emitidas := (mtransferencias_emitidas < transferencias_emitidas_ult_mes)]
dataset[, aumentou_transferencias_emitidas := (mtransferencias_emitidas > transferencias_emitidas_ult_mes)]
#quantidade de transferencias recebidas diminiuiu ou aumentou?
dataset[, transferencias_recibidas_ult_mes := shift(mtransferencias_recibidas, n = -1, type = "lead", fill = 0), by = numero_de_cliente]
dataset[, diminiuiu_transferencias_recibidas := (mtransferencias_recibidas < transferencias_recibidas_ult_mes)]
dataset[, aumentou_transferencias_recibidas := (mtransferencias_recibidas > transferencias_recibidas_ult_mes)]
#sacou mais do que depositou?
dataset[, sacou_mais_que_depositou := (ccajas_extracciones > ccajas_depositos)]
#depositou menos que o mes passado?
dataset[, depositou_menos_que_ult_mes := (ccajas_depositos < shift(ccajas_depositos, n = -1, type = "lead", fill = 0)), by = numero_de_cliente]
#sacou mais que o mes passado?
dataset[, sacou_mais_que_ult_mes := (ccajas_extracciones > shift(ccajas_extracciones, n = -1, type = "lead", fill = 0)), by = numero_de_cliente]
#numero de saques esta maior do que a média de saque dos ultimos 3 meses?
dataset[, media_3_meses_saq := shift(mean(ccajas_extracciones), n = -3, fill = 0), by = numero_de_cliente]
dataset[, saques_maior_que_media := ccajas_extracciones > media_3_meses_saq]

#numero de cheques depositados menor que a media dos cheques depositados dos ultimos tres meses
dataset[, media_3_meses_cd := shift(mean(ccheques_depositados), n = -3, fill = 0), by = numero_de_cliente]
dataset[, cheques_depositados_menor_que_media := ccheques_depositados < media_3_meses_cd]
#numero de cheques emitidos menor que a media dos cheques emitidos dos ultimos tres meses
dataset[, media_3_meses_ce := shift(mean(ccheques_emitidos), n = -3, fill = 0), by = numero_de_cliente]
dataset[, cheques_emitidos_menor_que_media := ccheques_emitidos < media_3_meses_ce]

#faz mais transacoes em outro caixa matm e matm_other
dataset[, otros_caixas := ifelse(dataset$matm < dataset$matm_other, 1, 0)]
