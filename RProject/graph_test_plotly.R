library(dplyr)
library(magrittr)
library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(webshot)


Sys.setenv("plotly_username" = "guigloriano")
Sys.setenv("plotly_api_key" = "VtjY2CY8A1qxNT69DeO8")
Sys.setenv("plotly_domain" = "http://mydomain.com")

csvPath <- setwd("D:\\github\\Tabelas_DynamoDB\\merge\\daily\\")
names <- list.files(pattern = "*.csv")

for(i in 1:length(names)){ 
  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  
  dataset <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))
  
  dados_graf = as.data.frame(dataset)
  
  graf_hour <- dataset$hora_minuto
  graf_temp <- dataset$temperatura
  graf_P_DC <- dataset$P_DC
  graf_P_AC <- dataset$P_AC
  
  data2 <- data.frame(graf_hour,graf_temp,graf_P_DC)

  p <- plot_ly(data2, x = ~graf_hour, y = ~graf_temp, type = 'bar', name = 'Temperatura') %>%
    add_trace(y = ~graf_P_DC, name = 'Potência DC') %>%
    add_trace(y = ~graf_P_AC, name = 'Potência AC') %>%
    layout(yaxis = list(title = 'Count'), barmode = 'group')
  
  dia <- dados_graf$dia_mes_ano[1]
  pathDest <- "D:/github/Tabelas_DynamoDB/merge/graph/teste/"
  fileDest <- paste(pathDest,  "graf_teste_", dia, ".html", sep = "")
  
  htmlwidgets::saveWidget(p, file = fileDest)
  
  plotly_IMAGE(p, format = "png", out_file = "output.png")
  
}

