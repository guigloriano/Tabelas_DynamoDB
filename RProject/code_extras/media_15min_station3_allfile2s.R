# List files
library(dplyr)
library(magrittr)
library(readr)
library("stringr")
library(plyr)  


files <- setwd("D:\\github\\Tabelas_DynamoDB\\ambientais_diario_min")
names <- list.files(pattern = "*.csv")

for(i in 1:length(names)){ 
  
  i <- 1
  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  
  x <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))
  
  x$hora_minuto <- str_pad(x$hora_minuto, width=6, side="left", pad="0")
  
  
  medida <- -1 
  
  for (j in 1:length(x$rainfall)){
    
    if ((x$rainfall[j] == x$rainfall[j+1])        ){
      
      x$rainfall[j+1] <- x$rainfall[j]
      x$rainfall[j] <- 0
      
      
    }
    
    j <- j+1
    
    
  }
  
  
  
  
  
}

