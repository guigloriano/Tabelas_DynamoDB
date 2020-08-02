library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
library(lubridate)
library(chron)
library(reshape2)       # funcao melt
library(ggplot2)        # funcao plot
library(scales)         # funcao date_breaks
library(hrbrthemes)     # funcao theme_ipsum


###### LEITURA DOS DADOS DA UFV-UFMS ######
caminhooTabelasUFV <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\06_combina_tabelas\\"
caminhoUFV <- setwd(caminhooTabelasUFV)
caminhoUFV <- setwd(caminhooTabelasUFV)
listaDados <- list.files(pattern = "*.csv")
csvUFV <- paste(caminhoUFV,  "/", listaDados, sep = "")
dias <- 1
csvCompleto_UFV <- c()
dataset2 <- c()
datasetIRR <- setNames(data.frame(matrix(ncol = 3, nrow = 150)), 
                       c("dia", "temp", "IRR"))

for(dias in 1:length(listaDados)){ 
  # for(dias in 1:1){   
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_character()))
  
  linha <- 1
  soma <- 0
  dataset2 <- na.omit(dataset)
  amostras <- nrow(dataset2)-1
  
  for(linha in 1:amostras){
    if ( dataset2$IRR[linha] > 0 ){
      soma <- soma+dataset2$IRR[linha]
    }
  }
  
  datasetAuxTemp <- na.omit(dataset)
  #datasetAuxTemp <- datasetAuxTemp[datasetAuxTemp$P_DC == 0,]
  
  
  soma <- round(soma / 60000, digits = 2)
  datasetIRR$dia[dias] <- dataset$dia_mes_ano[dias]
  datasetIRR$IRR[dias] <- soma
  
  datasetAuxTemp <- subset(dataset, dataset$P_DC !=0)
  
  datasetIRR$temp[dias] <- round(mean(datasetAuxTemp$temp), digits = 2)
  
  csvCompleto_UFV <- rbind(csvCompleto_UFV, dataset)
}
datasetIRR <- na.omit(datasetIRR)
datasetIRR$newDate <- parse_date_time(datasetIRR$dia, orders = c("ymd", "dmy", "mdy"))
datasetIRR$dia <- NULL





##### LEITURA DOS DADOS DE PRECIPITAçÃO #####
caminhoChuva <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\preciptacao_inmet\\"
cam_chuva <- setwd(caminhoChuva)
cam_chuva <- setwd(caminhoChuva)
csvChuva <- list.files(pattern = "*.csv")
chuva <- paste(cam_chuva,  "/", csvChuva, sep = "")
datasetChuva <- readr::read_csv(csvChuva, col_types = cols(precipitacao = col_character()))
datasetChuva$newDate <- parse_date_time(datasetChuva$data, orders = c("ymd", "dmy", "mdy"))
datasetChuva$precipitacao <- as.numeric(datasetChuva$precipitacao)
datasetChuva$data <- NULL


datasetAux1 <- merge(datasetChuva, datasetIRR, by.x = "newDate", 
                       by.y = "newDate", all.x = TRUE, all.y = FALSE)


#datasetAux1 <- na.omit(datasetAux1)
#fileDest <- paste("C:\\Users\\Guilherme\\Desktop\\Nova pasta (2)\\testeIRR.csv")
#write.csv(datasetAux1, fileDest) 


##### LEITURA DOS DADOS DO FRONIUS #####
caminhoFronius <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\z_fronius\\"
caminhoFroniusCSV <- setwd(caminhoFronius)
caminhoFroniusCSV <- setwd(caminhoFronius)
listaDados <- list.files(pattern = "*.csv")
csvFronius <- paste(caminhoFroniusCSV,  "/", listaDados, sep = "")
dias <- 1
csvCompleto_Fronius <- c()
for(dias in 1:length(listaDados)){ 
  # for(dias in 1:1){   
  dataset <- readr::read_csv(listaDados[dias], col_types = cols(hora_minuto = col_character()))
  csvCompleto_Fronius <- rbind(csvCompleto_Fronius, dataset)
}

csvCompleto_Fronius$newDate <- parse_date_time(csvCompleto_Fronius$`Data e horário`, orders = c("ymd", "dmy", "mdy"))


csvCompleto_Fronius$`Data e horário` <- NULL
csvCompleto_Fronius$`Energia por retificador alternado|Primo 8.2-1 (1) (# 1)` <- NULL
csvCompleto_Fronius$`Energia por retificador alternado por kWp|Primo 8.2-1 (1) (# 1)` <- NULL
csvCompleto_Fronius$X5 <- NULL

datasetAux1 <- merge(datasetAux1, csvCompleto_Fronius, by.x = "newDate", 
                     by.y = "newDate", all.x = TRUE, all.y = FALSE)


##### LEITURA DA MASSA ACUMULADA #####
massa_acumulada <- read.csv(file = 'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\m_acc\\DatasetMassaAcc.csv')

massa_acumulada$newDate <- parse_date_time(massa_acumulada$dia, orders = c("ymd", "dmy", "mdy"))

massa_acumulada$dia <- NULL  
massa_acumulada$Vd1 <- NULL
massa_acumulada$Vd2 <- NULL
massa_acumulada$Nloss <- NULL
massa_acumulada$Pd <- NULL
massa_acumulada$x_gauss <- NULL

datasetAux1 <- merge(datasetAux1, massa_acumulada, by.x = "newDate", 
                     by.y = "newDate", all.x = TRUE, all.y = FALSE)

names(datasetAux1)[names(datasetAux1) == "newDate"] <- "data"
names(datasetAux1)[names(datasetAux1) == "precipitacao"] <- "precip"# [mm]"
names(datasetAux1)[names(datasetAux1) == "temp"] <- "temp"# [C]"
names(datasetAux1)[names(datasetAux1) == "IRR"] <- "irr"# [kWh/m²]"
names(datasetAux1)[names(datasetAux1) == "Instalação total"] <- "producao"# [kwH]"
names(datasetAux1)[names(datasetAux1) == "m"] <- "m"# [ug/cm²]"

datasetAux1$producao <- as.numeric(datasetAux1$producao)
datasetAux1$data <- as.Date(datasetAux1$data)
df_test = datasetAux1 #[,!(names(datasetMerge2) %in% c("data","newDate", "dia", "Nloss", "Vd1", "Pd", "Vd2", "x_gauss"))]

df_test$m <- df_test$m*10
names(df_test)[names(df_test) == "m"] <- "m*10"# [ug/cm²]"


#### IMPRESSAO DO PRIMEIRO INTERVALO - 24/out à 06/nov #####
df_24out_06nov <- df_test %>% slice(24:37)
#df_test <- na.omit(df_test)
df_24out_06nov <- melt(data = df_24out_06nov, id.vars = "data", measure.vars = c("precip", "temp", "irr", "producao", "m*10"))
df_24out_06nov$data <- as.Date(df_24out_06nov$data)

gf_1prim <- ggplot(data = df_24out_06nov , aes(x=data, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 5*0:60, name = "Valores") +
  scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("days"))+
  theme(axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),) + 
  theme_ipsum() + ggtitle("Intervalo - 24/out à 06/nov") +
  theme(axis.text.x = element_text(angle = 60))

windows()
plot(gf_1prim)

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest,  "1_Intervalo.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
png(filename = fileDest, width = 1550, height = 720, units = 'px')
plot(gf_1prim)
dev.off()  




#### IMPRESSAO DO SEGUNDO  INTERVALO - 15/nov à 24/nov #####
df_15nov_24nov <- df_test %>% slice(46:55)
#df_test <- na.omit(df_test)
df_15nov_24nov <- melt(data = df_15nov_24nov, id.vars = "data", measure.vars = c("precip", "temp", "irr", "producao", "m*10"))
df_15nov_24nov$data <- as.Date(df_15nov_24nov$data)

gf_2prim <- ggplot(data = df_15nov_24nov , aes(x=data, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 5*0:60, name = "Valores") +
  scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("days"))+
  theme(axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),) + 
  theme_ipsum() + ggtitle("Intervalo - 15/nov à 24/nov") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_2prim)

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest,  "2_Intervalo.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
png(filename = fileDest, width = 1550, height = 720, units = 'px')
plot(gf_2prim)
dev.off()  





#### IMPRESSAO DO TERCEIRO INTERVALO - 27/fev à 19/mar #####
df_27fev_19mar <- df_test %>% slice(150:171)
#df_test <- na.omit(df_test)
df_27fev_19mar <- melt(data = df_27fev_19mar, id.vars = "data", measure.vars = c("precip", "temp", "irr", "producao", "m*10"))
df_27fev_19mar$data <- as.Date(df_27fev_19mar$data)

gf_3prim <- ggplot(data = df_27fev_19mar, aes(x=data, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 5*0:60, name = "Valores") +
  scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("days"))+
  theme(axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),) + 
  theme_ipsum() + ggtitle("Intervalo - 27/fev à 19/mar") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_3prim)

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest,  "3_Intervalo.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
png(filename = fileDest, width = 1550, height = 720, units = 'px')
plot(gf_3prim)
dev.off()  




#### IMPRESSAO DO QUARTO   INTERVALO - 08/abr à 13/abr #####
df_08abr_13abr <- df_test %>% slice(191:196)
#df_test <- na.omit(df_test)
df_08abr_13abr <- melt(data = df_08abr_13abr, id.vars = "data", measure.vars = c("precip", "temp", "irr", "producao", "m*10"))
df_08abr_13abr$data <- as.Date(df_08abr_13abr$data)

gf_4 <- ggplot(data = df_08abr_13abr, aes(x=data, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 5*0:60, name = "Valores") +
  scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("days"))+
  theme(axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),) + 
  theme_ipsum() + ggtitle("Intervalo - 08/abr à 13/abr") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_4)

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest,  "4_Intervalo.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
png(filename = fileDest, width = 1550, height = 720, units = 'px')
plot(gf_4)
dev.off()  





#### IMPRESSAO DO QUINTO   INTERVALO - 15/abr à 30/abr #####
df_15abr_30abr <- df_test %>% slice(198:213)
#df_test <- na.omit(df_test)
df_15abr_30abr <- melt(data = df_15abr_30abr, id.vars = "data", measure.vars = c("precip", "temp", "irr", "producao", "m*10"))
df_15abr_30abr$data <- as.Date(df_15abr_30abr$data)

gf_5 <- ggplot(data = df_15abr_30abr, aes(x=data, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 5*0:60, name = "Valores") +
  scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("days"))+
  theme(axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),) + 
  theme_ipsum() + ggtitle("Intervalo - 15/abr à 30/abr") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_5)

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest,  "5_Intervalo.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
png(filename = fileDest, width = 1550, height = 720, units = 'px')
plot(gf_5)
dev.off()  






#### IMPRESSAO TODO O PERIODO #####
#df_test <- na.omit(df_test)
df_test1 <- melt(data = df_test, id.vars = "data", measure.vars = c("precip", "temp", "irr", "producao", "m*10"))
df_test1$data <- as.Date(df_test1$data)

gf_6 <- ggplot(data = df_test1, aes(x=data, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 5*0:60, name = "Valores") +
  scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("3 days"))+
  theme(axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),) + 
  theme_ipsum() + ggtitle("Intervalo - Todo Período") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_6)

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest,  "6_Intervalo.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
png(filename = fileDest, width = 1550, height = 720, units = 'px')
plot(gf_6)
dev.off()  




#### IMPRESSAO PARA 2019 #####
#df_test <- na.omit(df_test)
df_2019 <- df_test %>% slice(1:92)
df_2019 <- melt(data = df_2019, id.vars = "data", measure.vars = c("precip", "temp", "irr", "producao", "m*10"))
df_2019$data <- as.Date(df_2019$data)

gf_7 <- ggplot(data = df_2019, aes(x=data, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 5*0:60, name = "Valores") +
  scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("3 days"))+
  theme(axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),) + 
  theme_ipsum() + ggtitle("Intervalo - 2019") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_7)

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest,  "7_Intervalo.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
png(filename = fileDest, width = 1550, height = 720, units = 'px')
plot(gf_7)
dev.off()  


#### IMPRESSAO PARA 2020 #####
#df_test <- na.omit(df_test)
df_2020 <- df_test %>% slice(93:n())
df_2020 <- melt(data = df_2020, id.vars = "data", measure.vars = c("precip", "temp", "irr", "producao", "m*10"))
df_2020$data <- as.Date(df_2020$data)

gf_8 <- ggplot(data = df_2020, aes(x=data, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 5*0:60, name = "Valores") +
  scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("2 days"))+
  theme(axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),) + 
  theme_ipsum() + ggtitle("Intervalo - 2020") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_8)

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest,  "8_Intervalo.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
png(filename = fileDest, width = 1550, height = 720, units = 'px')
plot(gf_8)
dev.off()  












#### IMPRESSAO PARA NOVEMBRO 2019 - Sem IRR #####
#df_test <- na.omit(df_test)
df_20201 <- df_test %>% slice(32:62)

df_20201$irr <- NULL
df_20201 <- melt(data = df_20201, id.vars = "data", measure.vars = c("precip", "temp", "producao", "m*10"))
df_20201$data <- as.Date(df_20201$data)

names(df_20201)[names(df_20201) == "variable"] <- "Variáveis"

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest,  "Nov2020.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
png(filename = fileDest, width = 1550, height = 720, units = 'px')


gf_9 <- ggplot(data = df_20201, aes(x=data, y = value, fill = Variáveis) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 5*0:60, name = "Valores") +
  scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("days"), expand = c(0,0))+
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black')) + 
  ggtitle("Novembro 2019") +
  theme(plot.title = element_text(hjust = 0.5, size=35), 
        axis.text.x = element_text(angle = 60, hjust = 1, size=25),
        axis.text.y = element_text(size = 25), 
        text = element_text(size=35))

plot(gf_9)
dev.off()  



df_test2019 <- datasetAux1
#df_test2019 <- na.omit(df_test2019)
df_test2019$data <- as.Date(df_test2019$data)
#fileDest <- paste("C:\\Users\\Guilherme\\Desktop\\Nova pasta (2)\\testeExcel.csv")
#write.csv(df_test2019, fileDest) 
  

df_test2019 <- df_test2019 %>% slice(1:92)
#df_test2019$producao <- as.numeric(df_test2019$producao)


#mean(na.omit(df_test$irr))


gf_test1 <- ggplot(data = df_test2019, aes(x = data, y = precip)) + #start plot by by plotting bars
  geom_bar(stat = "identity", width = 0.3, colour = 'blue') + 
  
  geom_line(data = df_test2019, aes(x = data, y = producao, group = 1),  
            size = 1, colour = 'red', inherit.aes = FALSE) +
  geom_point(data = df_test2019, aes(x = data, y = producao, group = 1),  
             size = 1.5, colour = 'red', inherit.aes = FALSE) +
  
  geom_line(data = df_test2019, aes(x = data, y = irr, group = 2),  
            size = 1, colour = 'green', inherit.aes = FALSE) +
  geom_point(data = df_test2019, aes(x = data, y = irr, group = 2),  
             size = 1.5, colour = 'green', inherit.aes = FALSE) +
  
  geom_line(data = df_test2019, aes(x = data, y = temp, group = 2),  
            size = 1, colour = 'purple', inherit.aes = FALSE) +
  geom_point(data = df_test2019, aes(x = data, y = temp, group = 2),  
             size = 1.5, colour = 'purple', inherit.aes = FALSE) +
  
  scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("3 days")) +
#  scale_y_discrete(breaks = 1*0:40) + 
  scale_y_continuous(breaks = 5*0:65) + 
  theme_ipsum() + 
  theme(
    axis.title.y = element_text(color = 'red', size=20, hjust = .5),
    axis.title.y.right = element_text(color = 'blue', size=20, hjust = .5, angle=270)) + 
  ggtitle("Intervalo - 2019") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 60))

plot(gf_test1)


pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest,  "9_teste.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
png(filename = fileDest, width = 1550, height = 720, units = 'px')
plot(gf_test1)
dev.off()  










csvCompleto_UFV$irr <- NULL
csvCompleto_UFV$massaPM1 <- NULL
csvCompleto_UFV$massaPM2 <- NULL
csvCompleto_UFV$massaPM4 <- NULL
csvCompleto_UFV$massaPM10 <- NULL
csvCompleto_UFV$numPM1 <- NULL
csvCompleto_UFV$numPM2 <- NULL
csvCompleto_UFV$numPM4 <- NULL
csvCompleto_UFV$numPM10 <- NULL
csvCompleto_UFV$tamanho_medio <- NULL
csvCompleto_UFV$vento_dir <- NULL
csvCompleto_UFV$rainfall <- NULL
csvCompleto_UFV$P_AC <- 
csvCompleto_UFV$I_AC <- NULL
csvCompleto_UFV$I_DC <- NULL
csvCompleto_UFV$V_AC <- NULL
csvCompleto_UFV$V_DC <- NULL








#celio <- df_test %>% slice(153:n())
#celio$`m*10` <- NULL
#fileDest <- paste("C:\\Users\\Guilherme\\Desktop\\Mes03e04.csv")
#write.csv(celio, fileDest) 






