# List files
library(dplyr)
library(magrittr)
library(readr)
library("stringr")
library(plyr)  


files <- setwd("D:\\github\\Tabelas_DynamoDB\\ambientais_diario_min")
names <- list.files(pattern = "*.csv")

for(i in 1:length(names)){ 
  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  
  x <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))
  
  x$hora_minuto <- str_pad(x$hora_minuto, width=6, side="left", pad="0")
  
  
  g <- dplyr::group_by(x, dia_mes_ano, h = substr(hora_minuto, 1, 2), 
                       m = floor(as.numeric(substr(hora_minuto, 3, 4))/15))
  
  gg <- dplyr::summarise(g, hora_minuto = dplyr::first(hora_minuto), 
                         irr = mean(irr), 
                         temp = mean(temp),
                         #umidade = mean(hum),
                         
                         rainfall = max(rainfall),
                        
                         massaPM1 = mean(massaPM1), 
                         massaPM2 = mean(massaPM2), 
                         massaPM4 = mean(massaPM4), 
                         massaPM10 = mean(massaPM10), 
                         numPM1 = mean(numPM1), 
                         numPM2 = mean(numPM2), 
                         numPM4 = mean(numPM4), 
                         numPM10 = mean(numPM10), 
                         tamanho_medio = mean(tamanho_medio), 
                         vento_dir=mean(vento_dir),
                         vento_vel=mean(vento_vel), 
                
                        
                         n = dplyr::n())
  
  y <- gg
  y$h <- NULL
  y$m <- NULL
  
  #write_csv(y,'C:\\Users\\LSCAD\\Documents\\Projeto\\Tabelas_DynamoDB\\ambientais_diarios_15min\\Ambientais15m_CG_20191004.csv')

  y$irr <-round(y$irr, digits = 2)
  y$temp <- round(y$temp, digits = 2)
  #y$preciptacao <-round(y$preciptacao, digits = 2)
  
    
  y$massaPM1 <- round(y$massaPM1, digits = 2)
  y$massaPM2 <- round(y$massaPM2, digits = 2)
  y$massaPM4 <- round(y$massaPM4, digits = 2)
  y$massaPM10 <- round(y$massaPM10, digits = 2)
  y$numPM1 <- round(y$numPM1, digits = 2)
  y$numPM2 <- round(y$numPM2, digits = 2)
  y$numPM4 <- round(y$numPM4, digits = 2)
  y$numPM10 <- round(y$numPM10, digits = 2)
  y$tamanho_medio <- round(y$tamanho_medio, digits = 2)
  y$vento_dir <- round(y$vento_dir, digits = 2)
  y$vento_vel <- round(y$vento_vel, digits = 2)
  
  salvarArq_path <- "D:\\github\\Tabelas_DynamoDB\\ambientais_diario_15min\\"
  salvarArq_name <- paste(salvarArq_path, "15min_", names[i], sep = "")
  write_csv(y, salvarArq_name)
  
}
