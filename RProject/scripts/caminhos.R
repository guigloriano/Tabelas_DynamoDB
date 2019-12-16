

inv_caminho <- "D:/github/Tabelas_DynamoDB/csv/inversor_diario_min/hora_corrigida/"
est_caminho <- "D:/github/Tabelas_DynamoDB/csv/ambientais_diario_min/"
merge_caminho <- "D:/github/Tabelas_DynamoDB/csv/teste_lm/merge/"
group_caminho <- "D:/github/Tabelas_DynamoDB/csv/teste_lm/group/"  

pathInv <- setwd(inv_caminho)
pathInv <- setwd(inv_caminho)
namesInv <- list.files(pattern = "*.csv")
filesInv <- paste(pathInv,  "/", namesInv, sep = "")

pathSta <- setwd(est_caminho)
pathSta <- setwd(est_caminho)
namesSta <- list.files(pattern = "*.csv")
filesSta <- paste(pathSta, "/", namesSta, sep = "")

pathMerge <- setwd(merge_caminho)
pathMerge <- setwd(merge_caminho)
namesMerge <- list.files(pattern = "*.csv")
filesMerge <- paste(pathMerge,  "/", namesMerge, sep = "")

pathGroup <- group_caminho

pathRegLin <- setwd(pathGroup )
pathRegLin <- setwd(pathGroup)
nameRegLin <- list.files(pattern = "*.csv")
filesRegLin <- paste(pathRegLin,  "/", nameRegLin, sep = "")


csvPath <- setwd(pathGroup)
names <- list.files(pattern = "*.csv")
