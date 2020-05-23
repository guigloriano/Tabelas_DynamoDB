library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

#caminhoDadosEstacao <- "D:\\github\\Tabelas_DynamoDB\\nodeJS\\csv_novo\\amb-novo\\"

caminhoDadosEstacao <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\amb\\"
caminhoCSV <- setwd(caminhoDadosEstacao)
caminhoCSV <- setwd(caminhoDadosEstacao)
listaDadosEstacao <- list.files(pattern = "*.csv")
csvEstacao <- paste(caminhoCSV,  "/", listaDadosEstacao, sep = "")

diaEst <- 1
amostrasEst_Diaria <- c()
amostrasGeralEst <- data.frame(NULL)
for(diaEst in 1:length(listaDadosEstacao)){ 
  #for(dia in 1:2){   
  datasetEstacao <- readr::read_csv(listaDadosEstacao[diaEst], col_types = cols(hora_minuto = col_character()))
  linhasEstacao <- nrow(datasetEstacao)
  amostrasEst_Diaria$dia_estacao <- datasetEstacao$dia_mes_ano[1]
  amostrasEst_Diaria$amostras_estacao <- linhasEstacao
  amostrasGeralEst <- rbind(amostrasGeralEst, amostrasEst_Diaria)
}


caminhoDadosInversor <- "D:\\github\\Tabelas_DynamoDB\\nodeJS\\csv_novo\\inv-novo\\"
caminhoInvCSV <- setwd(caminhoDadosInversor)
caminhoInvCSV <- setwd(caminhoDadosInversor)
listaDadosInversor <- list.files(pattern = "*.csv")
csvInversor <- paste(caminhoInvCSV,  "/", listaDadosInversor, sep = "")

diaInv <- 1
amostrasInv_Diaria <- c()
amostrasGeralInv <- data.frame(NULL)
for(diaInv in 1:length(listaDadosInversor)){ 
  #for(dia in 1:2){   
  datasetInversor <- readr::read_csv(listaDadosInversor[diaInv], col_types = cols(hora_minuto = col_character()))
  linhasInversor <- nrow(datasetInversor)
  amostrasInv_Diaria$dia_inversor <- datasetInversor$dia_mes_ano[1]
  amostrasInv_Diaria$amostras_inversor <- linhasInversor
  amostrasGeralInv <- rbind(amostrasGeralInv, amostrasInv_Diaria)
}

AnaliseAmostras <- cbind(amostrasGeralEst, amostrasGeralInv)


# caminhoPastaMerge <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_combinadas\\"
# caminhoMerge <- setwd(caminhoPastaMerge)
# caminhoMerge <- setwd(caminhoPastaMerge)
# listaDadosMerge <- list.files(pattern = "*.csv")
# csvMerge <- paste(caminhoMerge,  "/", listaDadosMerge, sep = "")

# diaMerge <- 1
# amostrasMerge_Diaria <- c()
# amostrasMerge_Geral <- data.frame(NULL)
# for(diaMerge in 1:length(listaDadosMerge)){ 
#  datasetMerge <- readr::read_csv(listaDadosMerge[diaMerge], col_types = cols(hora_minuto = col_character()))
#  linhasMerge <- nrow(datasetMerge)
#  amostrasMerge_Diaria$dia_merge <- datasetMerge$dia_mes_ano[1]
#  amostrasMerge_Diaria$amostras_merge <- linhasMerge
#  amostrasMerge_Geral <- rbind(amostrasMerge_Geral, amostrasMerge_Diaria)
# }

# AnaliseAmostras <- cbind(AnaliseAmostras, amostrasMerge_Geral)

write_csv(AnaliseAmostras,'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\Qtd_Amostras_Diaria-20200504.csv')

