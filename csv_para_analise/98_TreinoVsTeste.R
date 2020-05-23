library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
library(lubridate)
library(chron)

caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\09_combinacaofinal\\reduzidas\\"

caminhoCSV <- setwd(caminhoDados)
caminhoCSV <- setwd(caminhoDados)
listaDados <- list.files(pattern = "*.csv")
csvDados <- paste(caminhoCSV,  "/", listaDados, sep = "")

dias <- 1
csv_unificado <- c()
df_20menos <- c()
df_20mais <- c()

for(dias in 1:length(listaDados)){ 
  # for(dias in 1:1){   
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_character()))
  
  dataset$X1 <-NULL
  dataset <- subset(dataset, dataset$irr_inv !=0)
  dataset$dia <- dias
  dataset$m <- round(dataset$m, digits=5)
  num_linhas <- nrow(dataset)

  for(linha in 1:nrow(dataset)){
    dataset$TIME[linha] <- linha
  }
  
  amostras <- nrow(dataset)
  csv_unificado <- rbind(csv_unificado, dataset)
  
  if (dias < 21)
    df_20menos <- rbind(df_20menos, dataset)
    if (dias > 20)
    df_20mais <- rbind(df_20mais, dataset)
  
}
  
  df_20mais = as.data.frame(df_20mais)
  df_20menos = as.data.frame(df_20menos)
  
  df = as.data.frame(csv_unificado)
  #teste <- df[, c(17, 18, 4, 10, 16, 13, 2, 3, 5, 6, 7, 8, 9, 11, 12, 14, 15)]
  newdf <- df[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC", "dia_mes_ano", "hora_minuto",
                  "massaPM1", "numPM1", "massaPM2", "numPM2", "tamanho_medio", "vento_vel",
                  "vento_dir", "I_DC", "V_DC")]
  
  # 1X1            2dia_mes_ano         3hora_minuto         4irr_inv	
  # 5massaPM1      6numPM1              7massaPM2            8numPM2	
  # 9tamanho_medio 10temp               11vento_vel          12vento_dir	
  # 13P_DC         14I_DC	              15V_DC               16m	
  # 17dia          18TIME
  
  df_20mais <- df_20mais[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC")]
                  #     , "dia_mes_ano", "hora_minuto", "massaPM1", "numPM1", "massaPM2", 
                   #     "numPM2", "tamanho_medio", "vento_vel", "vento_dir", "I_DC", "V_DC")]
  
  df_20menos <- df_20menos[, c("dia", "TIME", "irr_inv", "temp", "m", "P_DC")]
                  #     , "dia_mes_ano", "hora_minuto", "massaPM1", "numPM1", "massaPM2", 
                  #     "numPM2", "tamanho_medio", "vento_vel", "vento_dir", "I_DC", "V_DC")]
  
  
  
  names(df_20mais)[names(df_20mais) == "dia"] <- "DIA"
  names(df_20mais)[names(df_20mais) == "irr_inv"] <- "IRR"
  names(df_20mais)[names(df_20mais) == "temp"] <- "TEMP"
  names(df_20mais)[names(df_20mais) == "m"] <- "MA"
  names(df_20mais)[names(df_20mais) == "P_DC"] <- "PDC"
  
  names(df_20menos)[names(df_20menos) == "dia"] <- "DIA"
  names(df_20menos)[names(df_20menos) == "irr_inv"] <- "IRR"
  names(df_20menos)[names(df_20menos) == "temp"] <- "TEMP"
  names(df_20menos)[names(df_20menos) == "m"] <- "MA"
  names(df_20menos)[names(df_20menos) == "P_DC"] <- "PDC"

  caminho_salvar <- "C:\\Users\\Guilherme\\Desktop\\"
  fileDest <- paste(caminho_salvar,  "/todos_dias3.csv", sep = "")
  write.csv(csv_unificado, fileDest) 

  fileDest <- paste(caminho_salvar,  "/Dias01a20.csv", sep = "")
  write.csv(df_20menos, fileDest) 
  
  fileDest <- paste(caminho_salvar,  "/Dias20a120.csv", sep = "")
  write.csv(df_20mais, fileDest) 

