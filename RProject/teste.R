library(dplyr)
library(readr)
library(stringr)
#arqDia <- 20191003
#csvPath <- "D:\\github\\Tabelas_DynamoDB\\inversor_diario_min\\inversor_1_ufms-"
#csvFile <- paste(csvPath, arqDia, ".csv", sep = "")

pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_min/hora_corrigida/")
pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_min/hora_corrigida/")
#pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_min/")
#pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_min/")
namesInv <- list.files(pattern = "*.csv")
filesInv <- paste(pathInv,  "/", namesInv, sep = "")

pathSta <- setwd("D:/github/Tabelas_DynamoDB/ambientais_diario_min/")
pathSta <- setwd("D:/github/Tabelas_DynamoDB/ambientais_diario_min/")
namesSta <- list.files(pattern = "*.csv")
filesSta <- paste(pathSta, "/", namesSta, sep = "")


for(i in 1:length(filesInv)){ 
  i = 1
  dfestacao <- filesSta[i]
  dfinversor <- filesInv[i]
  
  x_est <- readr::read_csv(dfestacao, col_types = cols(hora_minuto = col_character()))
  y_inv <- readr::read_csv(dfinversor, col_types = cols(hora_minuto = col_character()))
  
  x_est$hora_minuto <- str_pad(x_est$hora_minuto, width=6, side="left", pad="0")
  x_est$hora_minuto <- as.character(x_est$hora_minuto)
  
  y_inv$hora_minuto <- str_pad(y_inv$hora_minuto, width=6, side="left", pad="0")
  y_inv$hora_minuto <- as.character(y_inv$hora_minuto)
  
  z_merge <- merge.data.frame(x = x_est, y = y_inv, all = TRUE)
  
  #jointdataset <- merge(x, y, by = c('dia_mes_ano','hora_minuto'))
  #jointdataset$P_DC = jointdataset$I_DC * jointdataset$V_DC
  #jointdataset$P_DC <- round(jointdataset$P_DC, digits = 2)
  
  
  z_merge$P_DC = z_merge$I_DC * z_merge$V_DC
  
  
#  dia <- jointdataset$dia_mes_ano[i]
  #jointdataset$n.y <- NULL
  #jointdataset$n.x <- NULL
  #jointdataset$tipo <- NULL
  
  
  
  
  pathDest <- "D:/"
  fileDest <- paste(pathDest,  "teste_lm", ".csv", sep = "")
  
  write_csv(z_merge, fileDest)
  
  
  
}

