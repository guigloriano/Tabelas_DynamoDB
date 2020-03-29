library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
library(lubridate)

caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\amb\\"
#caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\inv\\"
#caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\amb\\interpoladas\\"

caminhoCSV <- setwd(caminhoDados)
caminhoCSV <- setwd(caminhoDados)
listaDados <- list.files(pattern = "*.csv")
csvDados <- paste(caminhoCSV,  "/", listaDados, sep = "")
dias <- 1
amostras_geral <- c()


for(dias in 1:length(listaDados)){ 
#for(dias in 1:1){   
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_character()))
  #dataset$X1 <- NULL
  
  dataset$irr = as.numeric(dataset$irr)
  dataset$massaPM1 = as.numeric(dataset$massaPM1)
  dataset$massaPM2 = as.numeric(dataset$massaPM2)
  dataset$massaPM4 = as.numeric(dataset$massaPM4)
  dataset$massaPM10 = as.numeric(dataset$massaPM10)
  dataset$tamanho_medio = as.numeric(dataset$tamanho_medio)
  dataset$temp = as.numeric(dataset$temp)
  dataset$numPM1 = as.numeric(dataset$numPM1)
  dataset$numPM2 = as.numeric(dataset$numPM2)
  dataset$numPM4 = as.numeric(dataset$numPM4)
  dataset$numPM10 = as.numeric(dataset$numPM10)
  dataset$vento_dir = as.numeric(dataset$vento_dir)
  dataset$vento_vel = as.numeric(dataset$vento_vel)
  dataset$rainfall = as.numeric(dataset$rainfall)
  
  
  
  
  dia <- dataset$dia_mes_ano[1]
  
  amostras <- nrow(dataset)
  linha_atual <- 1
  
  
  subAno <- substring(dataset$dia_mes_ano, 1, 4)
  subMes <- substring(dataset$dia_mes_ano, 5, 6)
  subDia <- substring(dataset$dia_mes_ano, 7, 8)
  
  dataset$hora_minuto <- str_pad(dataset$hora_minuto, 6, pad = "0")
  subHora <- substring(dataset$hora_minuto, 1, 2)
  subMin  <- substring(dataset$hora_minuto, 3, 4)
  subSeg  <- substring(dataset$hora_minuto, 5, 6)
  
  TempoAmostra <- paste(subAno, "-", subMes, "-", subDia, " " ,subHora, ":", subMin, ":", subSeg ,sep = "")
  dataset$Data <- strptime(TempoAmostra, format = "%Y-%m-%d %H:%M:%OS")

#  dataset$hora_minuto <- strptime(TempoAmostra, format = "%H:%M:%OS")
  dataset <- dataset[, c(17, 1, 2, 3, 4, 5, 8, 9, 12, 13, 14, 15, 6, 7, 10, 11, 16)]
  
  #dif_hora <- as.numeric(difftime(dataset$Data[linha_atual+1],dataset$Data[linha_atual]))
  
  if (amostras >= 1){
    while (linha_atual < amostras){
      
      dif_hora <- as.numeric(difftime(dataset$Data[linha_atual+1],dataset$Data[linha_atual]))
      
      # interpola o salto de 1 minuto das amostras diarias
      if (dif_hora == 2){
        amostras <- amostras+1
        dia_mes_ano = dataset$dia_mes_ano[1]
        
        hora_minuto = dataset$Data[linha_atual]+ 60
        hora_minuto <- format(hora_minuto, "%H%M%S")
        
#        subHoraModif1  <- substring(hora_minuto, 12, 13)
#        subHoraModif2  <- substring(hora_minuto, 15, 16)
#        subHoraModif3  <- substring(hora_minuto, 18, 19)
#        hora_minuto <- paste(subHoraModif1, subHoraModif2, subHoraModif3, sep = "")
        
        irr=        round ((dataset$irr[linha_atual]+dataset$irr[linha_atual+1])/2, digits = 2)
        massaPM1 =  round ((dataset$massaPM1[linha_atual]+dataset$massaPM1[linha_atual+1])/2, digits = 2)
        massaPM2 =  round ((dataset$massaPM2[linha_atual]+dataset$massaPM2[linha_atual+1])/2, digits = 2)
        massaPM4 =  round ((dataset$massaPM4[linha_atual]+dataset$massaPM4[linha_atual+1])/2, digits = 2)
        massaPM10 = round ((dataset$massaPM10[linha_atual]+dataset$massaPM10[linha_atual+1])/2, digits = 2)
        numPM1 =    round ((dataset$numPM1[linha_atual]+dataset$numPM1[linha_atual+1])/2, digits = 2)
        numPM2 =    round ((dataset$numPM2[linha_atual]+dataset$numPM2[linha_atual+1])/2, digits = 2)
        numPM4 =    round ((dataset$numPM4[linha_atual]+dataset$numPM4[linha_atual+1])/2, digits = 2)
        numPM10 =   round ((dataset$numPM10[linha_atual]+dataset$numPM10[linha_atual+1])/2, digits = 2)
   tamanho_medio =  round ((dataset$tamanho_medio[linha_atual]+dataset$tamanho_medio[linha_atual+1])/2, digits = 2)
        temp =      round ((dataset$temp[linha_atual]+dataset$temp[linha_atual+1])/2, digits = 2)
        vento_dir = round ((dataset$vento_dir[linha_atual]+dataset$vento_dir[linha_atual+1])/2, digits = 2)
        vento_vel = round ((dataset$vento_vel[linha_atual]+dataset$vento_vel[linha_atual+1])/2, digits = 2)
        rainfall =  round ((dataset$rainfall[linha_atual]+dataset$rainfall[linha_atual+1])/2, digits = 2)

        tempLinha <-  data.frame(dia_mes_ano, hora_minuto, irr, massaPM1, massaPM2, numPM1, numPM2, 
                           tamanho_medio, temp, vento_dir, vento_vel, 
                           massaPM4, massaPM10, numPM4, numPM10, rainfall)

        dataset$Data <- NULL
        datasetTemp <- rbind(dataset[1:linha_atual,],tempLinha,dataset[-(1:linha_atual),])
      }
      
      linha_atual<-linha_atual+1
    }
  }
  #dataset$X1 <- NULL
  
  
  # caminho para salvar os dados interpolados da estação de sujidade  
  caminho_salvar <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\amb\\interpoladas\\"
  fileDest <- paste(caminho_salvar,  "/amb_inter_", dia, ".csv", sep = "")
  write.csv(dataset, fileDest)
} 



