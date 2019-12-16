library(dplyr)
library(magrittr)
library(readr)
library("stringr")
library(plyr)  

files <- setwd("D:\\github\\Tabelas_DynamoDB\\ambientais_diario_min")
files = list.files(pattern="*.csv")

csv_mensal <- do.call("rbind", lapply(files, 
                                      function(x) read.csv(x,stringsAsFactors = FALSE, 
                                                           header = TRUE, na.strings = "n/a")))

write_csv(csv_mensal,"D:\\github\\Tabelas_DynamoDB\\temp\\tempStaMensal.csv")

ArqTem_path <- "D:\\github\\Tabelas_DynamoDB\\temp\\tempStaMensal.csv"

x <- readr::read_csv(ArqTem_path, col_types = cols(hora_minuto = col_character()))

#x$hora_minuto <- sprintf("%06s",x$hora_minuto)
x$hora_minuto <- str_pad(x$hora_minuto, width=6, side="left", pad="0")
#View(x$hora_minuto)

g <- dplyr::group_by(x, dia_mes_ano, h = substr(hora_minuto, 1, 2), 
                     m = floor(as.numeric(substr(hora_minuto, 3, 4))/15))

gg <- dplyr::summarise(g, hora_minuto = dplyr::first(hora_minuto), 
                       irradiacao = mean(irr), 
                       pm1_massa = mean(massaPM1), 
                       pm2_massa = mean(massaPM2), 
                       pm4_massa = mean(massaPM4), 
                       pm10_massa = mean(massaPM10), 
                       pm1_concentracao = mean(numPM1), 
                       pm2_concentracao = mean(numPM2), 
                       pm4_concentracao = mean(numPM4), 
                       pm10_concentracao = mean(numPM10), 
                       concentracao_media = mean(tamanho_medio), 
                       temperatura = mean(temp),
                       vento_dir=mean(vento_dir),
                       vento_vel=mean(vento_vel), n = dplyr::n())
y <- gg
y$h <- NULL
y$m <- NULL

write_csv(y,'D:\\github\\Tabelas_DynamoDB\\merge\\15min_ambientais-Mensal.csv')

