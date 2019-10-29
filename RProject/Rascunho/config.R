require(ggplot2)
require(lmtest)
library("stringr")


arqEst <- "~/Projeto/Tabelas_DynamoDB/RProject/DadosEst/ambientais_ufms_OK.csv"

arqInvFronius <- "~/Projeto/monitoramento-ufms"
  

  
nomeColEstUFMS <- c("dia_mes_ano", "hora_minuto", "massaPM1", "temp",
                "numPM4", "tipo", "irr", "rainfall", "numPM10",
                "tamanho_medio", "massaPM10", "numPM1", "massaPM2",
                "vento_dir", "hum", "massaPM4", "vento_vel",
                "numPM2")

data_estacao_inicio <- "20190905"
data_estacao_final  <- "20191010"

intervalo.estacaoUFMS.funcionamento <- "20191003"
intervalo.fim <- strftime(lubridate::today(), format = "%Y%m%d")

#----------------------------------------------
pathrepo <- "~/Projeto/monitoramento-ufms"

regex.inversor.path <- regex("InversorFronius")
regex.inversor.file <- regex(".*Relatório_diário_.*.csv")
columns.inversor <- c("timestamp","I_CA", "I_CC", "V_CA", "V_CC", "E_CA")

filter_data <- strptime("10/11/2018", format = "%d/%m/%Y")

x <- dirs
f1 <- function(x){
  if(!is.na(str_extract(x, regex.inversor.path))){
    files <- list.files(x, full.names = TRUE, include.dirs = TRUE, recursive = TRUE, pattern = regex.inversor.file)
    d <- do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE, header = FALSE, skip = 2, na.strings = "n/a")))
    return(d)
  }
  return()
}

f2 <- function(x){
  if(!is.na(str_extract(x, regex.estacao.path))){
    files <- list.files(x, full.names = TRUE, include.dirs = TRUE, recursive = TRUE, pattern = regex(".csv$"))
    files <- files[str_sub(files, -10, -5) >= strftime(filter_data, "%y%m%d")]
    d <- do.call("rbind", lapply(files, f3))
    return(d)
  }
  return()
}

f3 <- function(x){
  d <- read.csv(x, stringsAsFactors = FALSE, header = FALSE, skip = 3, na.strings = "n/a")
  d$data = str_sub(x, -10, -5)
  return(d)
}

g1_1 <- function(dia, d){
  p <- ggplot(data = d, aes(x = hora, group = 1)) + geom_line(aes(y=E_CC, color = "CC")) + geom_line(aes(y=E_CA, color ="CA")) + geom_line(aes(y=rad_sol*10.45, color = "Irradiação x kWp (ins)"), linetype = "dashed") + geom_line(aes(y=rad_sol_avg*10.45, color = "Irradiação x kWp (avg)"), linetype = "dashed") + geom_hline(yintercept = 8200)
  p <- p + xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), axis.text.y = element_text(size = 14), legend.position="bottom", legend.text = element_text(size = 16)) + scale_y_continuous(breaks = seq(0,15000,500), limits = c(0, 11000)) + scale_color_manual("", values=c("red", "blue", "black", "purple"))
  png(filename = paste("ufv-ufms2/imagens/potencia_radiacao_avg_ins/",gsub("[.]","_", dia),'.png', sep = ""), width = 1000, height = 537, units = 'px')
  print(p)
  dev.off()
}

g1_0 <- function(d){
  dias <- unique(d$data)
  for(dia in dias){
    g1_1(dia, d[d$data == dia,])
  }
}

#source("ufv-ufms2/codigos/f1.R")
#source("~/Projeto/dev-prediction-ai/rproject/analise_dados/ufv-ufms2/codigos/f1.R")

# Leitura dos dados do inversor
dirs <- list.dirs(pathrepo, full.names = TRUE, recursive = TRUE)
data.inversor <- do.call("rbind", lapply(dirs, f1))

data.inversor$V7 <- NULL
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

#----------------------------------------------

DadosEstUFMS <- read.csv(file=arqEst, sep = ",")

correlacaoEstUFMS <- cor(DadosEstUFMS)

write.table (correlacaoEstUFMS, "C:/Users/Guilherme/Documents/Projeto/Tabelas_DynamoDB/RProject/DadosEst/correlacaoEst.csv", sep = ",")
