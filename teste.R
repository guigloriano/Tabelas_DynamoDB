library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

newPath <- "D:\\github\\Tabelas_DynamoDB\\csv\\ambientais_diario_min\\testeEst\\"

# bloco para leitura dos arquivos .csvs do inversor
pathTeste <- setwd(newPath)
pathTeste <- setwd(newPath)
listTeste <- list.files(pattern = "*.csv")
fileTeste <- paste(pathTeste,  "/", listTeste, sep = "")


for (i in 1:124){
  
  dataset_temp <- readr::read_csv(fileTeste[i], col_types = cols(hora_minuto = col_character()))
  
  hora_inicio <- rbind(dataset_temp$hora_minuto[i])
  dia <- rbind(dataset_temp$dia_mes_ano[i])
  
  
}