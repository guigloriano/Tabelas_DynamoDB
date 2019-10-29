library(dplyr)


dfestacao <- 'C:\\Users\\LSCAD\\Documents\\Projeto\\Tabelas_DynamoDB\\ambientais_diarios_15min\\15min_ambientais_ufms-20191004.csv'
dfinversor <- 'C:\\Users\\LSCAD\\Documents\\Projeto\\Tabelas_DynamoDB\\inversor_diario_15min\\15min_inversor_1_ufms-20191004.csv'



x <- readr::read_csv(dfestacao, col_types = cols(hora_minuto = col_character()))
y <- readr::read_csv(dfinversor, col_types = cols(hora_minuto = col_character()))

z <- merge.data.frame(x = dfestacao, y = dfinversor)

jointdataset <- merge(x, y, by = c('dia_mes_ano','hora_minuto'))

jointdataset$P_DC = jointdataset$I_DC * jointdataset$V_DC

n.y <- NULL
n.x <- NULL

write_csv(jointdataset,'C:\\Users\\LSCAD\\Documents\\Projeto\\Tabelas_DynamoDB\\merge\\Ambientais_Inversor\\Merge.csv')

