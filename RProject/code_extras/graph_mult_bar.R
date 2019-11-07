library(dplyr)
library(magrittr)
library(readr)
library(tidyverse)
library(ggplot2)
library(reshape2)


csvPath <- setwd("D:\\github\\Tabelas_DynamoDB\\merge\\daily\\")
names <- list.files(pattern = "*.csv")

for(i in 1:length(names)){ 
  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  
  dataset <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))
  
  dados_graf = as.data.frame(dataset)

### GRAFICO BARRAS:  
  df1 <- data.frame(dados_graf$temperatura*100, dados_graf$P_AC, dados_graf$pm1_concentracao*100, dados_graf$preciptacao*1000, dados_graf$hora_minuto)
  df1 <- data.frame(dados_graf$temperatura, dados_graf$P_AC/100, dados_graf$pm1_concentracao, dados_graf$preciptacao, dados_graf$hora_minuto)
  df2 <- melt(df1, id.vars='dados_graf.hora_minuto')
  head(df2)
  
  p <- ggplot(df2, aes(x=dados_graf.hora_minuto, y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge') +
#    facet_wrap(~ variable) +
    theme(axis.text.x = element_text(angle = 90))
  plot(p)
  
  dia <- dados_graf$dia_mes_ano[1]
  
  pathDest <- "D:/github/Tabelas_DynamoDB/merge/graph/teste/graph_bar/"
  fileDest <- paste(pathDest,  "graph_", dia, ".png", sep = "")
  
  png(filename = fileDest, width = 1024, height = 800, units = 'px')
  print(p)
  dev.off()
  
}
