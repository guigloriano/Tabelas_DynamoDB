
library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)


caminhoDadosEstacao <- "D:\\github\\Tabelas_DynamoDB\\csv2\\dadosEstacao\\dias_completos\\"

caminhoCSV <- setwd(caminhoDadosEstacao)
caminhoCSV <- setwd(caminhoDadosEstacao)
listaDadosEstacao <- list.files(pattern = "*.csv")
csvEstacao <- paste(caminhoCSV,  "/", listaDadosEstacao, sep = "")

dia <- 1
datasetEstacao2 <- c()

for(dia in 1:length(listaDadosEstacao)){ 
#for(dia in 1:2){   
  datasetEstacao <- readr::read_csv(listaDadosEstacao[dia], col_types = cols(hora_minuto = col_character()))
  
  linhas <- nrow(datasetEstacao)
  
 # for(amostra in 1:linhas)
  
    
  
  
}


salvarArq_name <- paste("D:\\github\\Tabelas_DynamoDB\\csv2\\dadosEstacao\\DadosEstAnalisados", ".csv", sep = "")
write_csv(dados, salvarArq_name)
