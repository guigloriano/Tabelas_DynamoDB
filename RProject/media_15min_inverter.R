library(dplyr)
library(magrittr)
library(readr)

caminho_CSV <- "~/Projeto/Tabelas_DynamoDB/inversor_diario_min/inversor_1_ufms-20191025.csv"

x <- readr::read_csv(caminho_CSV, col_types = cols(hora_minuto = col_character()))

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

write_csv(y,'~/Projeto/Tabelas_DynamoDB/inversor_diario_15min/inversor15m_1_ufms-20191025.csv')
