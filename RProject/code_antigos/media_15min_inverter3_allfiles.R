library(dplyr)
library(magrittr)
library(readr)
library("stringr")
library(plyr)  


files <- setwd("D:\\github\\Tabelas_DynamoDB\\inversor_diario_min\\hora_corrigida\\")
names <- list.files(pattern = "*.csv")

for(i in 1:length(names)){ 
  assign(names[i],read.csv(names[i],skip=0, header=TRUE))
  
  x <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))
  
  x$hora_minuto <- str_pad(x$hora_minuto, width=6, side="left", pad="0")
  
  g <- dplyr::group_by(x, dia_mes_ano, h = substr(hora_minuto, 1, 2), 
                       m = floor(as.numeric(substr(hora_minuto, 3, 4))/15))
  
  gg <- dplyr::summarise(g, hora_minuto = dplyr::first(hora_minuto), 
                         IRR = mean(IRR),  
                         I_AC = mean(I_AC), 
                         V_AC = mean(V_AC), 
                         P_AC = mean(P_AC), 
                         I_DC = mean(I_DC), 
                         V_DC = mean(V_DC),
                         n = dplyr::n())
  y <- gg
  y$h <- NULL
  y$m <- NULL
  
  #write_csv(y,'C:\\Users\\LSCAD\\Documents\\Projeto\\Tabelas_DynamoDB\\ambientais_diarios_15min\\Ambientais15m_CG_20191004.csv')
  
  #write_csv(y,'C:\\Users\\LSCAD\\Documents\\Projeto\\Tabelas_DynamoDB\\ambientais_diarios_15min\\Ambientais15m_CG_20191004.csv')
  
  y$IRR <-round(y$IRR, digits = 2)
  y$I_AC <- round(y$I_AC, digits = 2)
  y$V_AC <- round(y$V_AC, digits = 2)
  y$P_AC <- round(y$P_AC, digits = 2)
  y$I_DC <- round(y$I_DC, digits = 2)
  y$V_DC <- round(y$V_DC, digits = 2)
  
  salvarArq_path <- "D:\\github\\Tabelas_DynamoDB\\inversor_diario_15min\\"
  salvarArq_name <- paste(salvarArq_path, "15min_", names[i], sep = "")
  write_csv(y, salvarArq_name)
  
}

