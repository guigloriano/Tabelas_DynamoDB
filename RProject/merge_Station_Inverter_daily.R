library(dplyr)
library(magrittr)
library(readr)
library("stringr")
library(plyr)  

pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_15min/")
pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_15min/")
namesInv <- list.files(pattern = "*.csv")
filesInv <- paste(pathInv,  "/", namesInv, sep = "")

pathSta <- setwd("D:/github/Tabelas_DynamoDB/ambientais_diario_15min/")
pathSta <- setwd("D:/github/Tabelas_DynamoDB/ambientais_diario_15min/")
namesSta <- list.files(pattern = "*.csv")
filesSta <- paste(pathSta, "/", namesSta, sep = "")


if (length(filesInv) > length(filesSta) || length(filesInv) == length(filesSta)){
  qtdFiles <- length(namesInv)
}

if (length(filesInv) < length(filesSta)){
  qtdFiles <- length(filesSta)
}

for(i in 1:qtdFiles){ 
  dfestacao <- filesSta[i]
  dfinversor <- filesInv[i]
  
  x <- readr::read_csv(dfestacao, col_types = cols(hora_minuto = col_character()))
  y <- readr::read_csv(dfinversor, col_types = cols(hora_minuto = col_character()))
#  z <- merge.data.frame(x = dfestacao, y = dfinversor)
  
  jointdataset <- merge(x, y, by = c('dia_mes_ano','hora_minuto'))
  jointdataset$P_DC = jointdataset$I_DC * jointdataset$V_DC
  
#  View(filesSta)
  jointdataset$P_DC <- round(jointdataset$P_DC, digits = 2)
  
  dia <- jointdataset$dia_mes_ano[i]
  
  jointdataset$n.y <- NULL
  jointdataset$n.x <- NULL
  
  # Inv_Est_Merge_dia.csv
  
  pathDest <- "D:/github/Tabelas_DynamoDB/merge/daily/"
  fileDest <- paste(pathDest,  "Inv_Est_Merge_", dia, ".csv", sep = "")
  
  write_csv(jointdataset, fileDest)
  
  
}  