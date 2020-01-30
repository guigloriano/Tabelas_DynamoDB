library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)

testeDiv <- "D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\merge\\"

# bloco para leitura dos arquivos .csvs do inversor
pathDiv <- setwd(testeDiv)
pathDiv <- setwd(testeDiv)
namesDiv <- list.files(pattern = "*.csv")
filesDiv <- paste(pathDiv,  "/", namesDiv, sep = "")


dataset_aux <- setNames(data.frame(matrix(ncol = 9, nrow = 1)), 
                        c("dia_mes_ano", "hora_minuto", "Vd1", "Pd", "Nloss", 
                          "Vd2", "m", "x_gauss", "SR" ))

dataset_save <- NULL
datasetDiv <- NULL

ListaVento <- NULL
ListaVentoMedia <- NULL
ListaMassa <- NULL
ListaMassaMedia <- NULL
ListaConcentracao <- NULL
ListaConcentracaoMedia <- NULL

aux_dia <- NULL

hora_inicial <- NULL
z_merge_total <- NULL

UAux <- NULL
RaAux <- NULL
RbAux <- NULL
VsAux <- NULL
VdAux <- NULL

newDataset <- data.frame(NULL)

dia_atual = 1
dia_inicio = 1
dia_final = 84
# summary(reg_linear)
# dia_final = length(filesDiv)


dia_quebra <- NULL
hora_quebra <- 1

teste <- 1

# namesDiv[15]
# for (i in 1:length(namesDiv)){
for (dia_atual in dia_inicio:dia_final){
  
  dia_quebra <- (dia_atual - 1)*24
  
  dataset_temp <- readr::read_csv(namesDiv[dia_atual], col_types = cols(hora_minuto = col_character()))
  
  if (dataset_temp$hora_minuto[1] <= "010000"){
    hora_inicial <- "000000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "010000")
    hora_quebra <- dia_quebra + 1
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "020000"){
    hora_inicial <- "010000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "020000")
    hora_quebra <- dia_quebra + 2
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "030000"){
    hora_inicial <- "020000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "030000")
    hora_quebra <- dia_quebra + 3
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "040000"){
    hora_inicial <- "030000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "040000")
    hora_quebra <- dia_quebra + 4
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "050000"){
    hora_inicial <- "040000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "050000")
    hora_quebra <- dia_quebra + 5
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "060000"){
    hora_inicial <- "050000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "060000")
    hora_quebra <- dia_quebra + 6
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }

  if (dataset_temp$hora_minuto[1] <= "070000"){
    hora_inicial <- "060000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "070000")
    hora_quebra <- dia_quebra + 7
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "080000"){
    hora_inicial <- "070000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "080000")
    hora_quebra <- dia_quebra + 8
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
      
  if (dataset_temp$hora_minuto[1] <= "090000"){
    hora_inicial <- "080000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "090000")
    hora_quebra <- dia_quebra + 9
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
        
  if (dataset_temp$hora_minuto[1] <= "100000"){
    hora_inicial <- "090000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "100000")
    hora_quebra <- dia_quebra + 10
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "110000"){
    hora_inicial <- "100000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "110000")
    hora_quebra <- dia_quebra + 11
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "120000"){
    hora_inicial <- "110000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "120000")
    hora_quebra <- dia_quebra + 12
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "130000"){
    hora_inicial <- "120000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "130000")
    hora_quebra <- dia_quebra + 13
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "140000"){
    hora_inicial <- "130000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "140000")
    hora_quebra <- dia_quebra + 14
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "150000"){
    hora_inicial <- "140000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "150000")
    hora_quebra <- dia_quebra + 15
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "160000"){
    hora_inicial <- "150000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "160000")
    hora_quebra <- dia_quebra + 16
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "170000"){
    hora_inicial <- "160000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "170000")
    hora_quebra <- dia_quebra + 17
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "180000"){
    hora_inicial <- "170000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "180000")
    hora_quebra <- dia_quebra + 18
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "190000"){
    hora_inicial <- "180000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "190000")
    hora_quebra <- dia_quebra + 19
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "200000"){
    hora_inicial <- "190000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "200000")
    hora_quebra <- dia_quebra + 20
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "210000"){
    hora_inicial <- "200000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "210000")
    hora_quebra <- dia_quebra + 21
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "220000"){
    hora_inicial <- "210000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "220000")
    hora_quebra <- dia_quebra + 22
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "230000"){
    hora_inicial <- "220000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "230000")
    hora_quebra <- dia_quebra + 23
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "235900"){
    hora_inicial <- "230000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "235959")
    hora_quebra <- dia_quebra + 24
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }

  
}

dataset_save <- rbind(dataset_save, dataset_aux)
dataset_save <- na.omit(dataset_save)#aux_dia[i] <- datasetDiv$dia_mes_ano[1]


horaCaminho <- "D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\group"

# bloco para leitura dos arquivos .csvs do inversor
pathHORA <- setwd(horaCaminho)
pathHORA <- setwd(horaCaminho)
namesHORA <- list.files(pattern = "*.csv")
filesHORA <- paste(pathHORA,  "/", namesHORA, sep = "")

aux_i <- 1
aux_j <- 1
aux_k <- length(filesHORA)


dataset_HORA_aux <- NULL

for (aux_i in aux_j:aux_k){
  dataset_HORA <- readr::read_csv(namesHORA[aux_i], col_types = cols(hora_minuto = col_character()))
  dataset_HORA_aux <- rbind(dataset_HORA_aux, dataset_HORA)
}

zy_merge <- merge(x = dataset_HORA_aux, y = dataset_save, by=c("dia_mes_ano", "hora_minuto"), all = TRUE)

salvarArq_name <- paste("D:\\teste", ".csv", sep = "")
write_csv(zy_merge, salvarArq_name)

zy_merge <- na.omit(zy_merge)
#zy_merge[is.na(zy_merge)] <- 0

#modelo <- zy_merge$P_AC ~ zy_merge$irr_est + zy_merge$irr_inv + zy_merge$temp + zy_merge$numPM1 + zy_merge$massaPM1 + 
#  zy_merge$numPM2 + zy_merge$massaPM2 + zy_merge$vento_vel + zy_merge$vento_dir + zy_merge$DVr1 + zy_merge$DVr2 + 
#  zy_merge$IDV1 + zy_merge$IDV2 + zy_merge$Vd1 + zy_merge$Pd + zy_merge$Nloss + zy_merge$Vd2 + zy_merge$m + 
#  zy_merge$x_gauss + zy_merge$SR

modelo <- zy_merge$P_AC ~ zy_merge$irr_inv + zy_merge$temp + zy_merge$numPM1 + zy_merge$massaPM1 + 
  zy_merge$vento_vel + zy_merge$Vd1 + zy_merge$Pd + zy_merge$m + zy_merge$x_gauss + zy_merge$SR


reg_linear <- lm(modelo, data = zy_merge, na.action=na.omit )  
summary(reg_linear) 

dfy_cor <- data.frame(zy_merge)
dfy_cor$dia_mes_ano <- NULL
dfy_cor$hora_minuto <- NULL
dfy_cor$rainfall <- NULL
dfy_cor$irr_est <- NULL
dfy_cor$numPM2 <- NULL
dfy_cor$massaPM2 <- NULL
dfy_cor$vento_dir <- NULL
dfy_cor$DVr1 <- NULL
dfy_cor$DVr2 <- NULL
dfy_cor$IDV1 <- NULL
dfy_cor$IDV2 <- NULL
dfy_cor$Nloss <- NULL
dfy_cor$Vd2 <- NULL
corr <- cor(dfy_cor[, ])
