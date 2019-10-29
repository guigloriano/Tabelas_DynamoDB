#source("ufv-ufms2/codigos/f1.R")
source("~/Projeto/Tabelas_DynamoDB/RProject/a_f1.R")

# Leitura dos dados do inversor
dirs <- list.dirs(pathrepo, full.names = TRUE, recursive = TRUE)


data.inversor <- do.call("rbind", lapply(dirs, f1))
data.inversor$V7 <- NULLdirs


t <- strptime(data.inversor$V1, "%d.%m.%Y %H:%M")
data.inversor$V1 <- as.numeric(strftime(t, "%Y%m%d%H%M%S"))
names(data.inversor) <- columns.inversor
data.inversor$E_CC <- data.inversor$I_CC * data.inversor$V_CC
data.inversor$data <- strftime(t, "%Y.%m.%d")
data.inversor$hora <- strftime(t, "%H:%M:%S")
data.inversor <- data.inversor[data.inversor$data >= strftime(filter_data, "%Y.%m.%d"),]

data.estacao <- do.call("rbind", lapply(dirs, f2))
t = strptime(paste(data.estacao$V1, data.estacao$data), "%H:%M %y%m%d")
data.estacao$V1 = as.numeric(strftime(t, "%Y%m%d%H%M%S"))
names(data.estacao) = columns.estacao
data.estacao$data = strftime(t, "%Y.%m.%d")
data.estacao$hora = strftime(t, "%H:%M:%S")

for(c in columns.estacao.tonumeric){
  data.estacao[c] <- as.numeric(data.estacao[,c])
}

data = merge(x = data.inversor, y = data.estacao, by = "timestamp", all = TRUE)
t = strptime(as.character(data$timestamp), "%Y%m%d%H%M%S")
data$data = strftime(t, "%Y.%m.%d")
data$hora = strftime(t, "%H:%M:%S")
data$data.x = NULL
data$data.y = NULL
data$hora.x = NULL
data$hora.y = NULL

vars = c("timestamp", "data", "hora", "I_CC", "V_CC", "E_CA", "E_CC", "temp", "rad_sol_avg", "rad_sol", "umidade", "vel_vento", "dir_vento",  "raj_vento", "raw_barom", "chuva_tot", "barom_sl")
dataset = data[,vars]
dataset = dataset[complete.cases(dataset),]
dataset = dataset[dataset$rad_sol_avg > 0,]

g = group_by(dataset, data)
dataset_dia = summarise(g, E_CA = sum(E_CA), E_CC = sum(E_CC)/4, rad_sol_avg = sum(rad_sol_avg)/4, rad_sol = sum(rad_sol)/4, q = n(), chuva_tot = sum(chuva_tot), temp_max = max(temp), temp_min = min(temp), temp = mean(temp), umidade_max = max(umidade), umidade_min = min(umidade), umidade = mean(umidade), dir_vento = mean(dir_vento), vel_vento = mean(vel_vento), raj_vento = mean(raj_vento), raw_barom = mean(raw_barom), barom_sl = mean(barom_sl))

g = group_by(dataset, data, hora = substr(hora, 1, 2))
dataset_hora = summarise(g, E_CA = sum(E_CA), E_CC = mean(E_CC), rad_sol_avg = mean(rad_sol_avg), rad_sol = mean(rad_sol), chuva_tot = sum(chuva_tot), temp_max = max(temp), temp_min = min(temp), temp = mean(temp), umidade_max = max(umidade), umidade_min = min(umidade), umidade = mean(umidade), dir_vento = mean(dir_vento), vel_vento = mean(vel_vento), raj_vento = mean(raj_vento), raw_barom = mean(raw_barom), barom_sl = mean(barom_sl))

rm(filter_data, t, g, dirs, f1, f2, f3, vars, regex.estacao.path, regex.inversor.file, regex.inversor.path, pathrepo, c, columns.estacao.tonumeric)
