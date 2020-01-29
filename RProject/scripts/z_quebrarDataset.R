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

j = 1
k = 1

# namesDiv[15]
# for (i in 1:length(namesDiv)){
for (i in j:k){
  
  dataset_temp <- readr::read_csv(namesDiv[i], col_types = cols(hora_minuto = col_character()))
  
  if (dataset_temp$hora_minuto[1] <= "010000"){
    hora_inicial <- "000000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "010000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "020000"){
    hora_inicial <- "010000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "020000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "030000"){
    hora_inicial <- "020000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "030000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "040000"){
    hora_inicial <- "030000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "040000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "050000"){
    hora_inicial <- "040000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "050000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "060000"){
    hora_inicial <- "050000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "060000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }

  if (dataset_temp$hora_minuto[1] <= "070000"){
    hora_inicial <- "060000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "070000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "080000"){
    hora_inicial <- "070000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "080000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
      
  if (dataset_temp$hora_minuto[1] <= "090000"){
    hora_inicial <- "080000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "090000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
        
  if (dataset_temp$hora_minuto[1] <= "100000"){
    hora_inicial <- "090000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "100000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "110000"){
    hora_inicial <- "100000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "110000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "120000"){
    hora_inicial <- "110000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "120000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "130000"){
    hora_inicial <- "120000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "130000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "140000"){
    hora_inicial <- "130000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "140000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "150000"){
    hora_inicial <- "140000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "150000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "160000"){
    hora_inicial <- "150000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "160000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "170000"){
    hora_inicial <- "160000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "170000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "180000"){
    hora_inicial <- "170000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "180000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "190000"){
    hora_inicial <- "180000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "190000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "200000"){
    hora_inicial <- "190000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "200000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "210000"){
    hora_inicial <- "200000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "210000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "220000"){
    hora_inicial <- "210000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "220000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "230000"){
    hora_inicial <- "220000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "230000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "235900"){
    hora_inicial <- "230000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "235959")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  
  # dataset_save <- rbind(dataset_save, dataset_aux)
  
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

zy_merge <- merge.data.frame(x = dataset_HORA_aux, y = dataset_save, by='dia_mes_ano', by='hora_minuto', all.x = TRUE)

#salvarArq_ate18h30 <- paste("D:\\github\\Tabelas_DynamoDB\\", "dataset_04-10_part1.csv", sep = "")
#write_csv(ate18h30, salvarArq_ate18h30)

#salvarArq_ate00h00 <- paste("D:\\github\\Tabelas_DynamoDB\\", "dataset_04-10_part2.csv", sep = "")
#write_csv(ate00h00, salvarArq_ate00h00)