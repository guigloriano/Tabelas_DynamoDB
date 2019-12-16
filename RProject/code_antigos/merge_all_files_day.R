library(dplyr)
library(readr)
library(stringr)
#arqDia <- 20191003
#csvPath <- "D:\\github\\Tabelas_DynamoDB\\inversor_diario_min\\inversor_1_ufms-"
#csvFile <- paste(csvPath, arqDia, ".csv", sep = "")

pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_15min/")
pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_15min/")
#pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_min/")
#pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_min/")
namesInv <- list.files(pattern = "*.csv")
filesInv <- paste(pathInv,  "/", namesInv, sep = "")

pathSta <- setwd("D:/github/Tabelas_DynamoDB/ambientais_diario_15min/")
pathSta <- setwd("D:/github/Tabelas_DynamoDB/ambientais_diario_15min/")
namesSta <- list.files(pattern = "*.csv")
filesSta <- paste(pathSta, "/", namesSta, sep = "")


for(i in 1:length(filesInv)){ 
  dfestacao <- filesSta[i]
  dfinversor <- filesInv[i]
  
  x <- readr::read_csv(dfestacao, col_types = cols(hora_minuto = col_character()))
  y <- readr::read_csv(dfinversor, col_types = cols(hora_minuto = col_character()))
  
  
  x$hora_minuto <- str_pad(x$hora_minuto, width=6, side="left", pad="0")
  x$hora_minuto <- as.character(x$hora_minuto)
  
  y$hora_minuto <- str_pad(y$hora_minuto, width=6, side="left", pad="0")
  y$hora_minuto <- as.character(y$hora_minuto)
  
  z <- merge.data.frame(x = x, y = y)
  
  jointdataset <- merge(x, y, by = c('dia_mes_ano','hora_minuto'))
  jointdataset$P_DC = jointdataset$I_DC * jointdataset$V_DC
  
  jointdataset$P_DC <- round(jointdataset$P_DC, digits = 2)
  
  dia <- jointdataset$dia_mes_ano[i]
  
  jointdataset$n.y <- NULL
  jointdataset$n.x <- NULL
  
  jointdataset$tipo <- NULL
  
  pathDest <- "D:/github/Tabelas_DynamoDB/merge/daily/"
  fileDest <- paste(pathDest,  "merge_tables_", dia, ".csv", sep = "")
  
  write_csv(jointdataset, fileDest)
  
  
  
}

