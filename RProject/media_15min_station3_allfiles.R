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
                         irradiacao = mean(irr), 
                         temperatura = mean(temp),
                         #umidade = mean(hum),
                         
                         preciptacao = max(rainfall),
                        
                         pm1_massa = mean(massaPM1), 
                         pm2_massa = mean(massaPM2), 
                         pm4_massa = mean(massaPM4), 
                         pm10_massa = mean(massaPM10), 
                         pm1_concentracao = mean(numPM1), 
                         pm2_concentracao = mean(numPM2), 
                         pm4_concentracao = mean(numPM4), 
                         pm10_concentracao = mean(numPM10), 
                         concentracao_media = mean(tamanho_medio), 
                         vento_dir=mean(vento_dir),
                         vento_vel=mean(vento_vel), 
                
                        
                         n = dplyr::n())
  
  y <- gg
  y$h <- NULL
  y$m <- NULL
  
  #write_csv(y,'C:\\Users\\LSCAD\\Documents\\Projeto\\Tabelas_DynamoDB\\ambientais_diarios_15min\\Ambientais15m_CG_20191004.csv')

  y$irradiacao <-round(y$irradiacao, digits = 2)
  y$temperatura <- round(y$temperatura, digits = 2)
  #y$preciptacao <-round(y$preciptacao, digits = 2)
  
    
  y$pm1_massa <- round(y$pm1_massa, digits = 2)
  y$pm2_massa <- round(y$pm2_massa, digits = 2)
  y$pm4_massa <- round(y$pm4_massa, digits = 2)
  y$pm10_massa <- round(y$pm10_massa, digits = 2)
  y$pm1_concentracao <- round(y$pm1_concentracao, digits = 2)
  y$pm2_concentracao <- round(y$pm2_concentracao, digits = 2)
  y$pm4_concentracao <- round(y$pm4_concentracao, digits = 2)
  y$pm10_concentracao <- round(y$pm10_concentracao, digits = 2)
  y$concentracao_media <- round(y$concentracao_media, digits = 2)
  y$vento_dir <- round(y$vento_dir, digits = 2)
  y$vento_vel <- round(y$vento_vel, digits = 2)
  
  salvarArq_path <- "D:\\github\\Tabelas_DynamoDB\\ambientais_diario_15min\\"
  salvarArq_name <- paste(salvarArq_path, "15min_", names[i], sep = "")
  write_csv(y, salvarArq_name)
  
}
