library(dplyr)
library(magrittr)
library(readr)
library("stringr")
library(plyr)  

#temp <- setwd("~/Projeto/Tabelas_DynamoDB/inversor_diario_min/")
#temp = list.files(pattern="*.csv")
#myfiles = lapply(temp, read.delim)
#View(myfiles)
#-------------------------------------#
#temp <- setwd("~/Projeto/Tabelas_DynamoDB/inversor_diario_min/")
#temp = list.files(pattern="*.csv")
#list2env(
#  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
#         read.csv), envir = .GlobalEnv)

files <- setwd("~/Projeto/Tabelas_DynamoDB/inversor_diario_min/")
files = list.files(pattern="*.csv")
csv_mensal <- do.call("rbind", lapply(files, 
                             function(x) read.csv(x,stringsAsFactors = FALSE, 
                                                  header = TRUE, na.strings = "n/a")))
write_csv(csv_mensal,'~/Projeto/Tabelas_DynamoDB/inversor_diario_15min/temp.csv')

ArqTem_path <- "~/Projeto/Tabelas_DynamoDB/inversor_diario_15min/temp.csv"

x <- readr::read_csv(ArqTem_path, col_types = cols(hora_minuto = col_character()))
#x$hora_minuto <- sprintf("%06s",x$hora_minuto)

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

write_csv(y,'~/Projeto/Tabelas_DynamoDB/inversor_diario_15min/inversor15m_ufms-Mensal.csv')

