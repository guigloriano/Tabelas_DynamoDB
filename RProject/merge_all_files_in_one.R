library(tidyverse)

setwd("D:\\github\\Tabelas_DynamoDB\\merge\\all")

filenames <- list.files(path="D:\\github\\Tabelas_DynamoDB\\merge\\all",pattern="*.csv")
print(filenames)

fullpath=file.path("D:\\github\\Tabelas_DynamoDB\\merge\\all",filenames)
print(fullpath)



for(i in 1:length(fullpath)){ 
  
  assign(fullpath[i],read.csv(fullpath[i],skip=1, header=TRUE))
  
  x <- readr::read_csv(fullpath[i], col_types = cols(hora_minuto = col_character()))
  
  
}


dataset <- do.call("rbind",lapply(filenames,FUN=function(files){ read.csv(files)}))


# remocao de campos nao necessarios para analise do Prof. Erlandson

dataset$tipo <- NULL
dataset$massaPM4 <- NULL
dataset$massaPM10 <- NULL
dataset$numPM4 <- NULL
dataset$numPM10 <- NULL
dataset$I_DC <- NULL
dataset$I_AC <- NULL
dataset$V_DC <- NULL
dataset$V_AC <- NULL
dataset$IRR <- NULL
dataset$P_AC <- NULL 
dataset$tamanho_medio <- NULL

pathDest <- "D:\\github\\Tabelas_DynamoDB\\merge\\all\\one\\"
fileDest <- paste(pathDest,  "all_files_inv_est",".csv", sep = "")

write_csv(dataset, fileDest)
