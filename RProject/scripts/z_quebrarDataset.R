testeDiv <- "D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\merge\\"

# bloco para leitura dos arquivos .csvs do inversor
pathDiv <- setwd(testeDiv)
pathDiv <- setwd(testeDiv)
namesDiv <- list.files(pattern = "*.csv")
filesDiv <- paste(pathDiv,  "/", namesDiv, sep = "")


dataset_aux <- setNames(data.frame(matrix(ncol = 9, nrow = 1)), 
                        c("dia", "hora", "Vd1", "Pd", "Nloss", 
                          "Vd2", "m", "x_gauss", "SR" ))

ListaVento <- NULL
ListaVentoMedia <- NULL
ListaMassa <- NULL
ListaMassaMedia <- NULL
ListaConcentracao <- NULL
ListaConcentracaoMedia <- NULL
aux_dia <- NULL

UAux <- NULL
RaAux <- NULL
RbAux <- NULL
VsAux <- NULL
VdAux <- NULL

j = 1
k = 1


#for (i in 1:length(namesDiv)){
for (i in j:k){
    
  #i = 1
  
  
  dataset_temp<- readr::read_csv(namesDiv[i], col_types = cols(hora_minuto = col_character()))
  #aux_dia[i] <- datasetDiv$dia_mes_ano[1]

  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "010000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "020000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "030000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "040000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "050000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "060000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "070000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "080000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "090000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "100000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "110000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "120000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "130000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "140000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "150000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "160000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "170000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "180000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "190000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "200000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "210000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "220000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "230000")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
  
  datasetDiv <- filter(dataset_temp, dataset_temp$hora_minuto <= "235959")
  source("D:\\github\\Tabelas_DynamoDB\\RProject\\scripts\\z_testeSerieTemporal.R")
}


#salvarArq_ate18h30 <- paste("D:\\github\\Tabelas_DynamoDB\\", "dataset_04-10_part1.csv", sep = "")
#write_csv(ate18h30, salvarArq_ate18h30)

#salvarArq_ate00h00 <- paste("D:\\github\\Tabelas_DynamoDB\\", "dataset_04-10_part2.csv", sep = "")
#write_csv(ate00h00, salvarArq_ate00h00)