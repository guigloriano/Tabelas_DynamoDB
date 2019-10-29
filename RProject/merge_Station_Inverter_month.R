library(dplyr)
library(readr)
#arqDia <- 20191003
#csvPath <- "D:\\github\\Tabelas_DynamoDB\\inversor_diario_min\\inversor_1_ufms-"
#csvFile <- paste(csvPath, arqDia, ".csv", sep = "")


dfestacao <- 'D:\\github\\Tabelas_DynamoDB\\ambientais_diario_15min\\15m_ambientais-Mensal.csv'
dfinversor <- 'D:\\github\\Tabelas_DynamoDB\\inversor_diario_15min\\15min_inversor-Mensal.csv'

x <- readr::read_csv(dfestacao, col_types = cols(hora_minuto = col_character()))
y <- readr::read_csv(dfinversor, col_types = cols(hora_minuto = col_character()))

z <- merge.data.frame(x = dfestacao, y = dfinversor)

jointdataset <- merge(x, y, by = c('dia_mes_ano','hora_minuto'))

jointdataset$P_DC = jointdataset$I_DC * jointdataset$V_DC

n.y <- NULL
n.x <- NULL

write_csv(jointdataset,'D:\\github\\Tabelas_DynamoDB\\merge\\Merge-Outubro.csv')

