testeDiv <- "D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\merge\\"

# bloco para leitura dos arquivos .csvs do inversor
pathDiv <- setwd(testeDiv)
pathDiv <- setwd(testeDiv)
namesDiv <- list.files(pattern = "*.csv")
filesDiv <- paste(pathDiv,  "/", namesDiv, sep = "")


dataset_aux <- setNames(data.frame(matrix(ncol = 9, nrow = 1)), 
                        c("dia", "hora", "Vd1", "Pd", "Nloss", 
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
hora_final <- NULL

UAux <- NULL
RaAux <- NULL
RbAux <- NULL
VsAux <- NULL
VdAux <- NULL

j = 1
k = 30

# namesDiv[15]
# for (i in 1:length(namesDiv)){
for (i in j:k){
    
  #i = 15
  
  
  dataset_temp<- readr::read_csv(namesDiv[i], col_types = cols(hora_minuto = col_character()))
  dataset_temp <- na.omit(dataset_temp)#aux_dia[i] <- datasetDiv$dia_mes_ano[1]

  if (dataset_temp$hora_minuto[1] <= "010000"){
    hora_final <- "010000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "010000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "020000"){
    hora_final <- "020000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "020000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "030000"){
    hora_final <- "030000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "030000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "040000"){
    hora_final <- "040000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "040000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "050000"){
    hora_final <- "050000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "050000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "060000"){
    hora_final <- "060000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "060000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }

  if (dataset_temp$hora_minuto[1] <= "070000"){
    hora_final <- "070000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "070000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "080000"){
    hora_final <- "080000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "080000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
      
  if (dataset_temp$hora_minuto[1] <= "090000"){
    hora_final <- "090000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "090000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
        
  if (dataset_temp$hora_minuto[1] <= "100000"){
    hora_final <- "100000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "100000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "110000"){
    hora_final <- "110000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "110000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "120000"){
    hora_final <- "120000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "120000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "130000"){
    hora_final <- "130000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "130000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "140000"){
    hora_final <- "140000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "140000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "150000"){
    hora_final <- "150000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "150000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "160000"){
    hora_final <- "160000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "160000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "170000"){
    hora_final <- "170000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "170000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "180000"){
    hora_final <- "180000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "180000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "190000"){
    hora_final <- "190000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "190000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "200000"){
    hora_final <- "200000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "200000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "210000"){
    hora_final <- "210000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "210000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "220000"){
    hora_final <- "220000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "220000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "230000"){
    hora_final <- "230000"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "230000")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  if (dataset_temp$hora_minuto[1] <= "235900"){
    hora_final <- "235900"
    datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "235959")
    source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  }
  
  dataset_save <- rbind(dataset_save, dataset_aux)
  
}



#salvarArq_ate18h30 <- paste("D:\\github\\Tabelas_DynamoDB\\", "dataset_04-10_part1.csv", sep = "")
#write_csv(ate18h30, salvarArq_ate18h30)

#salvarArq_ate00h00 <- paste("D:\\github\\Tabelas_DynamoDB\\", "dataset_04-10_part2.csv", sep = "")
#write_csv(ate00h00, salvarArq_ate00h00)