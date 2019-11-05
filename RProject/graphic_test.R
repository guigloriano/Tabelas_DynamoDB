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
  #df1 <- data.frame(dados_graf$temperatura*100, dados_graf$P_AC, dados_graf$pm1_concentracao*100,
  #                  dados_graf$preciptacao*1000, dados_graf$hora_minuto)
  
#  df1 <- data.frame(dados_graf$temperatura, dados_graf$P_AC/100, dados_graf$pm1_concentracao, 
#                    dados_graf$preciptacao, dados_graf$hora_minuto)
  
#  df2 <- melt(df1, id.vars='dados_graf.hora_minuto')
#  head(df2)
  

#    p <- ggplot(df2, aes(x=dados_graf.hora_minuto, y=value, fill=variable)) +
#    geom_bar(stat='identity', position='dodge') +
#        facet_wrap(~ variable) +
#    theme(axis.text.x = element_text(angle = 90))

  p <- ggplot(data = dados_graf, mapping = aes(x = hora_minuto)) + 
    geom_line(aes(y=temperatura, color="temperatura", group=1)) +
    
    geom_line(aes(y=P_AC/100, color="P_AC", group=2)) + 
    geom_line(aes(y=P_DC/100, color="P_DC", group=3)) + 
    
    geom_line(aes(y=pm1_concentracao, color="pm1_concentracao", group=4)) + 
    geom_line(aes(y=pm2_concentracao, color="pm2_concentracao", group=5)) + 
  
    geom_line(aes(y=vento_dir/2, color="vento_dir", group=6)) + 
    geom_line(aes(y=preciptacao, color="preciptacao", group=7)) + 
    
    geom_line(aes(y=irradiacao/10, color="irr_est", group=8)) + 
    geom_line(aes(y=IRR/10, color="irr_inv", group=9, )) +
    
    scale_y_continuous(sec.axis = sec_axis(~.*10, name = "POT. AC")) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(y = "Temperatura, PM1 (Concentração) & Prec. Pluviométrica",
         x = "Horas do Dia",
         colour = "Variáveis") 

  
  dia <- dados_graf$dia_mes_ano[1]
  
  pathDest <- "D:/github/Tabelas_DynamoDB/merge/graph/teste/"
  fileDest <- paste(pathDest,  "Todas_Variaveis_", dia, ".png", sep = "")
  
  png(filename = fileDest, width = 1024, height = 800, units = 'px')
  plot(p)
  dev.off()
  
}
