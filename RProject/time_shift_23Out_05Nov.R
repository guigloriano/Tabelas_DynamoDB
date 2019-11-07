# List files
library(dplyr)
library(magrittr)
library(readr)
library("stringr")
library(plyr)  

files <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_15min/")
names <- list.files(pattern = "*.csv")

for(i in 21:34){ 

  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  
  x <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))
  
  x$hora_minuto <- str_pad(x$hora_minuto, width=6, side="left", pad="0")

  t <- strptime(paste(x$hora_minuto), "%H%M%S")
  t <- t - lubridate::as.period(1, unit = "hours")
  x$hora_minuto <- as.numeric(strftime(t, "%H%M%S"))

  salvarArq_path <- "D:\\github\\Tabelas_DynamoDB\\inversor_diario_15min\\"
  salvarArq_name <- paste(salvarArq_path, names[i], sep = "")
  write_csv(x, salvarArq_name)

}