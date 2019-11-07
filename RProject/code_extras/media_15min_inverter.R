library(dplyr)
library(magrittr)
library(readr)

arqDia <- 20191003
csvPath <- "D:\\github\\Tabelas_DynamoDB\\inversor_diario_min\\inversor_1_ufms-"
csvFile <- paste(csvPath, arqDia, ".csv", sep = "")
#View(csvFile)

x <- readr::read_csv(csvFile, col_types = cols(hora_minuto = col_character()))

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

csvPath_dest <- "D:\\github\\Tabelas_DynamoDB\\inversor_diario_15min\\15min_inversor_1_ufms-"
csvFile_dest <- paste(csvPath_dest, arqDia, ".csv", sep = "")
#View(csvFile_dest)

write_csv(y, csvFile_dest)

