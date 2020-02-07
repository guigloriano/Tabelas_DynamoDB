
library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)


caminhoDadosEstacao <- "D:\\github\\Tabelas_DynamoDB\\csv2\\dadosEstacao\\todos_dias\\"

caminhoCSV <- setwd(caminhoDadosEstacao)
caminhoCSV <- setwd(caminhoDadosEstacao)
listaDadosEstacao <- list.files(pattern = "*.csv")
csvEstacao <- paste(caminhoCSV,  "/", listaDadosEstacao, sep = "")

amostra <- c()
dia <- c()
hora_inicio <- c()
hora_final <- c()
qtd_amostras <- c()


for(i in 1:length(listaDadosEstacao)){ 
  
  datasetEstacao <- readr::read_csv(listaDadosEstacao[i], col_types = cols(hora_minuto = col_character()))
  
  
  linhasDataset <- nrow(datasetEstacao)
  
  if (linhasDataset == 0){
    linhasDataset = "000000"
  }
  
  amostra       <- c(amostra, i)
  dia           <- c(dia, datasetEstacao$dia_mes_ano[1])
  hora_inicio   <- c(hora_inicio, datasetEstacao$hora_minuto[1])
  hora_final    <- c(hora_final, datasetEstacao$hora_minuto[linhasDataset])
  qtd_amostras  <- c(qtd_amostras, linhasDataset)
  
}

dados <- data.frame(amostra, dia, hora_inicio, hora_final, qtd_amostras)

salvarArq_name <- paste("D:\\github\\Tabelas_DynamoDB\\csv2\\dadosEstacao\\DadosEstAnalisados", ".csv", sep = "")
write_csv(dados, salvarArq_name)
