library(dplyr)
library(magrittr)
library(readr)
library(tidyverse)
library(ggplot2)

csvPath <- setwd("D:\\github\\Tabelas_DynamoDB\\merge\\daily\\")
names <- list.files(pattern = "*.csv")
#View(csvFile)

for(i in 1:length(names)){ 
  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  
  dataset <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))

  dados_graf = as.data.frame(dataset)
  
  graf <- ggplot(data = dados_graf, mapping = aes(x = hora_minuto)) + 
                      geom_line(aes(y=numPM1, color="pm1_concentracao", group=1)) +
                      geom_line(aes(y=numPM2, color="pm2_concentracao", group=2)) + 
                      geom_line(aes(y=numPM4, color="pm4_concentracao", group=3)) + 
                      geom_line(aes(y=numPM10, color="pm10_concentracao", group=4)) + 
                      geom_line(aes(y=vento_dir/2, color="vento_dir", group=5)) + 
                      geom_line(aes(y=vento_vel, color="vento_vel", group=5)) + 
#                      geom_line(aes(y=P_DC/100, color="P_DC", group=5)) + 
    
                      theme(axis.text.x = element_text(angle = 90)) +
                      labs(x = "Hora", y = "Concentracao de Particulado [ug/cm³]") +
                      ggtitle("Distribuicao de Massa") 
  
  dia <- dados_graf$dia_mes_ano[1]
  
  pathDest <- "D:/github/Tabelas_DynamoDB/merge/graph/pms/"
  fileDest <- paste(pathDest,  "graf_particulados_", dia, ".png", sep = "")
  
  png(filename = fileDest, width = 1000, height = 537, units = 'px')
  print(graf)
  dev.off()
  
}
      