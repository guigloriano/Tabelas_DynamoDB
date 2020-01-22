

inv_caminho <- "D:/github/Tabelas_DynamoDB/csv/inversor_diario_min/hora_corrigida/"
est_caminho <- "D:/github/Tabelas_DynamoDB/csv/ambientais_diario_min/dias_completos/"
merge_caminho <- "D:/github/Tabelas_DynamoDB/csv/teste_lm/merge/"
group_caminho <- "D:/github/Tabelas_DynamoDB/csv/teste_lm/group/"  

# bloco para leitura dos arquivos .csvs do inversor
pathInv <- setwd(inv_caminho)
pathInv <- setwd(inv_caminho)
namesInv <- list.files(pattern = "*.csv")
filesInv <- paste(pathInv,  "/", namesInv, sep = "")


# bloco para leitura dos arquivos .csvs da estacao
pathSta <- setwd(est_caminho)
pathSta <- setwd(est_caminho)
namesSta <- list.files(pattern = "*.csv")
filesSta <- paste(pathSta, "/", namesSta, sep = "")

# bloco para leitura do caminho que os arquivos combinados
# serao salvos
pathMerge <- setwd(merge_caminho)
pathMerge <- setwd(merge_caminho)
namesMerge <- list.files(pattern = "*.csv")
filesMerge <- paste(pathMerge,  "/", namesMerge, sep = "")

# agrupamento dos arquivos em registros de 1hr
pathGroup <- group_caminho

pathRegLin <- setwd(pathGroup )
pathRegLin <- setwd(pathGroup)
nameRegLin <- list.files(pattern = "*.csv")
filesRegLin <- paste(pathRegLin,  "/", nameRegLin, sep = "")

# leitura dos arquivos pra regressaoLinear
csvPath <- setwd(pathGroup)
names <- list.files(pattern = "*.csv")
