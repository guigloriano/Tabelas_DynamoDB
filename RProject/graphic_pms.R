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
                      geom_line(aes(y=pm1_massa, color="pm1_massa", group=1)) +
                      geom_line(aes(y=pm2_massa, color="pm2_massa", group=1)) + 
                      geom_line(aes(y=pm4_massa, color="pm4_massa", group=1)) + 
                      geom_line(aes(y=pm10_massa, color="pm10_massa", group=1)) + 
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
      