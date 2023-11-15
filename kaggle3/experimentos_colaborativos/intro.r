rm(list = ls()) # remove all objects
gc() # garbage collection

#carregar dados do arquivo 
#setwd('/Users/maiconfialho/Documents/Mestrado/2023-2/dmeyf2023/kaggle3')
setwd("C:/Users/maico/Documents/Mestrado/dmeyf2023/kaggle3/") # Establezco el Directorio de Trabajo

#df_kaggle3 <- read.csv('./datos/competencia_03_crudo.csv.gz')

library(data.table)
library(dplyr)
df_kaggle3 <- fread("./experimentos_colaborativos/datos/competencia_03_ec.csv.gz")

head(df_kaggle3)

#ver a quantidade de nulos em cada coluna
colSums(is.na(df_kaggle3))

#ver a porcentagem de nulos em cada coluna por ordem decrescente
sort(colSums(is.na(df_kaggle3))/nrow(df_kaggle3), decreasing = TRUE)

#armazenar os nomes das colunas com valores nulos mas com menos de 30% de nulos
# Determine a proporção de valores não nulos em cada coluna
prop_nao_nulos <- sapply(df_kaggle3, function(col) mean(!is.na(col)))

# Selecione os nomes das colunas que têm pelo menos 30% de valores não nulos e pelo menos um valor nulo e inclua a coluna numero_de_ciente e foto_mes
#nomes_colunas <- names(prop_nao_nulos[prop_nao_nulos > 0.7 & prop_nao_nulos < 1]) 
nomes_colunas <- names(prop_nao_nulos[prop_nao_nulos >= 0.3 & colSums(is.na(df_kaggle3)) > 0])
colunas_selecionadas <- c(nomes_colunas, 'numero_de_cliente', 'foto_mes')



#verificar o tipo dos datos das colunas com nulos, exibir apenas os tipos unicos
#unique(sapply(df_kaggle_nulos, class))
#ver a distribuicao dos dados das colunas com valores nulos que estao na variavel colunas_nulos
df_kaggle_nulos <- subset(df_kaggle3, select = colunas_selecionadas)

#listar os valores unicos de mes_foto que possuem valores nulos
unique(df_kaggle_nulos$foto_mes[is.na(df_kaggle_nulos$numero_de_cliente)])
summary(df_kaggle_nulos)

#exibir um grafico de barras com a quantidade de nulos em cada coluna
library(visdat)
vis_dat(df_kaggle_nulos, warn_large_data = FALSE)

#graficar adistribuicao dos dados das colunas com valores nulos


#verificar a correlacao entre as colunas com valores nulos
cor(df_kaggle_nulos)

#heatmap para a correlaao
library(reshape2)
library(ggplot2)
melted_cormat <- melt(cor(df_kaggle_nulos))
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#usar o marginplot com as colunas do df_kaggle_nulos
library(ggcorrplot)
ggcorrplot(cor(df_kaggle_nulos), hc.order = TRUE, type = "lower", lab = TRUE)





