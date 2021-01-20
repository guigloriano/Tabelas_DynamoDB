library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)
require(tidyverse)
library(chron)
library(scales)

caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\ambientais\\"

caminhoAmbientais <- setwd(caminhoDados)
caminhoAmbientais <- setwd(caminhoDados)
listaDadosAmbientais <- list.files(pattern = "*.csv")
csvDadosAmbientais <- paste(caminhoAmbientais,  "/", listaDadosAmbientais, sep = "")

dias <- 1
datasetAux <- NULL

dia_atual <- 1
dia_inicio <- 1

#dias <- 2

for(dias in 1:length(listaDadosAmbientais)){ 
  df <- listaDadosAmbientais[dias]
  dataset <- readr::read_csv(df, col_types = cols(hora_minuto = col_character()))
  qtd_amostras <- nrow(dataset)
  
  if (qtd_amostras != 0){
    amostras <- 1
    datasetAux <- rbind(datasetAux, dataset)
    cont_dias <- dias
  }
  
  if (qtd_amostras == 0){
    
    texto <- as.numeric(substring(df, 17, 24))

    dataset <- setNames(data.frame(matrix(ncol = 16, nrow = 1)), 
                        c("dia_mes_ano", "hora_minuto", "irr", "massaPM1", "massaPM2", 
                          "massaPM4","massaPM10", "numPM1", "numPM2", "numPM4", "numPM10",
                          "tamanho_medio", "temp", "vento_dir", "vento_vel", "rainfall"))
    
    dataset <- rbind(dataset, list(texto, "000000", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                     deparse.level = 1)
    
    dataset <- dataset[-c(1),]
    datasetAux <- rbind(datasetAux, dataset)
  }
  
}
  datasetAux$newDate <- as.Date.character(datasetAux$dia_mes_ano, "%Y%m%d")
  datasetAux$medNumPM1 <- datasetAux$numPM1
  datasetAux$medNumPM2 <- datasetAux$numPM2
  
  dt_teste2 <- dplyr::group_by(datasetAux, year = year(newDate), week = week(newDate))
  
  dt_teste2sum <- dplyr::summarise(dt_teste2, 
                                   massaPM1 = sum(massaPM1), 
                                   massaPM2 = sum(massaPM2), 
                                   
                                   numPM1 = sum(numPM1), 
                                   numPM2 = sum(numPM2),
                                   
                                   medianumPM1 = mean(medNumPM1, na.rm=TRUE), 
                                   medianumPM2 = mean(medNumPM2, na.rm=TRUE),

                                   p_dia = first(dia_mes_ano),
                                   u_dia = last(dia_mes_ano),
                                   newDate = first(newDate),
                                   n = dplyr::n())
    

#  gf_teste1 <- ggplot() +  
#    geom_point(data = dt_teste2sum, aes(newDate, numPM1, group = 1), colour = 'blue', size = 1) +
#    geom_line(data = dt_teste2sum, aes(newDate, numPM1, group = 1), colour = 'blue', size = 1) +
#    geom_point(data = dt_teste2sum, aes(newDate, numPM2*100, group = 2), colour = 'red', size = 1) +
#    geom_line(data = dt_teste2sum, aes(newDate, numPM2*100, group = 2), colour = 'red', size = 1) +
#    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
#    scale_y_continuous(
#      name = "PM1 [#g/cm設",
#      sec.axis = sec_axis(~./100, name = "PM2 [#/cm設")
#    ) + theme_ipsum() + 
#    theme(
#      axis.title.y = element_text(color = 'blue', size=13, h=.5),
#      axis.title.y.right = element_text(color = 'red', size=13, h=.5)
#    ) + labs(x = "Semana") +
#    ggtitle("Soma do Particulado [#g/cm設") +
#    theme(axis.text.x = element_text(angle = 60))
  
  gf_teste2 <- ggplot()  + 
    geom_point(data = dt_teste2sum, aes(newDate, medianumPM1, group = 3,), colour = 'darkgreen', size = 1) +
    geom_line(data = dt_teste2sum, aes(newDate, medianumPM1, group = 3), colour = 'darkgreen', size = 1) +
    geom_point(data = dt_teste2sum, aes(newDate, medianumPM2*100, group = 4), colour = 'purple', size = 1) +
    geom_line(data = dt_teste2sum, aes(newDate, medianumPM2*100, group = 4), colour = 'purple', size = 1) +
    scale_x_date(name = "Semanas", labels = date_format("%d %b"), breaks = date_breaks("week")) +
    scale_y_continuous(
      name = "PM1 [#/cm設",
      sec.axis = sec_axis(~./100, name = "PM2 [#/cm設"),
      breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
    ) + theme_ipsum() + 
    theme(
      axis.title.y = element_text(color = 'darkgreen', size=20, h=.5),
      axis.title.y.right = element_text(color = 'purple', size=20, h=.5),
      axis.title.x = element_text(color = "black", size=20,  h=.5)
    ) + ggtitle("M嶮ia da Concentra誽o de Particulado [#/cm設") +
     theme(text = element_text(size=1),
          axis.text.x = element_text(size=20, angle=45), 
          axis.text.y = element_text(size=20))
 
#   grid_dia <- grid.arrange(gf_teste1, gf_teste2, 
  grid_dia <- grid.arrange(gf_teste2, 
                           ncol=, nrow=1, top="Qtd. Total e M嶮ia da Concentra誽o de Particulados por semana")
  
  
  pathDest <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\m_acc\\"
  fileDest <- paste(pathDest,  "SomaMedia_Part_Semana.png", sep = "")
  #  png(filename = fileDest, width = 1280, height = 720, units = 'px')
  png(filename = fileDest, width = 1550, height = 720, units = 'px')
  plot(grid_dia)
  dev.off()  

  