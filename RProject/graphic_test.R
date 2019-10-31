library(dplyr)
library(magrittr)
library(readr)
library(tidyverse)
library(ggplot2)


csvPath <- setwd("D:\\github\\Tabelas_DynamoDB\\merge\\daily\\")
names <- list.files(pattern = "*.csv")

for(i in 1:length(names)){ 
  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  
  dataset <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))
  
  dados_graf = as.data.frame(dataset)
  
  graf <- ggplot(data = dados_graf, 
                 mapping = aes(x = hora_minuto, y = preciptacao, 
                               fill=pm1_massa)) + 
    geom_bar(stat="identity")   +       
    #    geom_bar(aes(y=preciptacao, color="precitacao", group=1)) +
#    geom_bar(aes(y=pm1_massa, color="pm1_massa", group=2)) + 
#    geom_line(aes(y=pm4_massa, color="pm4_massa", group=3)) + 
#    geom_line(aes(y=pm10_massa, color="pm10_massa", group=4)) + 
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "Hora", y = "Concentracao de Particulado [ug/cm³]") +
    ggtitle("Índice Pluviométrico [mm]") 
  
  dia <- dados_graf$dia_mes_ano[1]
  
  pathDest <- "D:/github/Tabelas_DynamoDB/merge/graph/part_precip/"
  fileDest <- paste(pathDest,  "graf_particulados_", dia, ".png", sep = "")
  
  png(filename = fileDest, width = 1000, height = 537, units = 'px')
  print(graf)
  dev.off()
  
}
