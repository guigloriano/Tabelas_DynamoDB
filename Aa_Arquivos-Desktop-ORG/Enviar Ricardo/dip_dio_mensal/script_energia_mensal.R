library(dplyr)
library(lubridate)
library(scales)
library(reshape2)
library(ggplot2)



teste <- numeric()
dias_new <- length(Dip_list)
teste <- data.frame("DIA" = seq(1,dias_new), "Predita" = Dip_list, "Observada" = Dio_list)


testeDias <- read.csv(file = 'C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\TodosDias2.csv', 
                      header = TRUE)

testeDias$X <- NULL

testeDias3 <- merge(teste, testeDias, by.x = "DIA", 
                      by.y = "dia", all.x = TRUE, all.y = FALSE)

testeDias3$TIME <- NULL
testeDias3$irr_inv <- NULL
testeDias3$temp <- NULL
testeDias3$m <- NULL
testeDias3$P_DC <- NULL


testeDias3 <- distinct(testeDias3)

testeDiasOK <- testeDias3
testeDiasOK$DIA <- NULL


testeDiasOK$dia_mes_ano  <- parse_date_time(testeDiasOK$dia_mes_ano, orders = c("ymd", "dmy", "mdy"))
testeDiasOK$dia_mes_ano <- as.Date(testeDiasOK$dia_mes_ano)

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\dip_dio_mensal\\"


#### DIP, DIO - Mensais (Outubro/19) ####

  nome_arquivo <- paste ("1_Out19_energia.png")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')
  
  diasMes <- numeric()
  diasMes <- data.frame("dia" = seq(as.Date("2019/10/1"), as.Date("2019/10/31"), "days"))
  diasOutubro <- testeDiasOK[1:8,]
  diasOutubro <- merge(diasOutubro, diasMes, by.x = "dia_mes_ano", by.y = "dia", all.x = TRUE, all.y = TRUE)
  diasOutubro[is.na(diasOutubro)] <- 0
  
  grupoOut <- melt(data = diasOutubro, id.vars = "dia_mes_ano", measure.vars = c("Predita", "Observada"))
  names(grupoOut)[names(grupoOut) == "variable"] <- "Energia"
  gf_predOut <- ggplot(data = grupoOut , aes(x=dia_mes_ano, y = value, fill = Energia) ) + 
    geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(breaks = 50*0:900, name = "Energia") +
    scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("days"), expand = c(0,0)) +
    theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
    #  theme_ipsum() + 
    ggtitle("Dias - Outubro/19") +
    theme(axis.text.x = element_text(angle = 60))
  plot(gf_predOut)
    
  dev.off()

  
#### DIP, DIO - Mensais (NOvemrbo/19) ####
    
  nome_arquivo <- paste ("2_Nov19_energia.png")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')

  diasMes <- numeric()
  diasMes <- data.frame("dia" = seq(as.Date("2019/11/1"), as.Date("2019/11/30"), "days"))
  diasNovembro <- testeDiasOK[9:33,]
  diasNovembro <- merge(diasNovembro, diasMes, by.x = "dia_mes_ano", by.y = "dia", all.x = TRUE, all.y = TRUE)
  diasNovembro[is.na(diasNovembro)] <- 0
    
  grupoNov <- melt(data = diasNovembro, id.vars = "dia_mes_ano", measure.vars = c("Predita", "Observada"))
  names(grupoNov)[names(grupoNov) == "variable"] <- "Energia"
  gf_predNov <- ggplot(data = grupoNov , aes(x=dia_mes_ano, y = value, fill = Energia) ) + 
    geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(breaks = 50*0:900, name = "Energia") +
    scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("days"), expand = c(0,0)) +
    theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
    #  theme_ipsum() + 
    ggtitle("Dias - Novembro/19") +
    theme(axis.text.x = element_text(angle = 60))
  plot(gf_predNov)
    
  dev.off()


#### DIP, DIO - Mensais (Dezembro/19) ####
    
  nome_arquivo <- paste ("3_Dez19_energia.png")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')

  diasMes <- numeric()
  diasMes <- data.frame("dia" = seq(as.Date("2019/12/1"), as.Date("2019/12/31"), "days"))
  diasDezembro <- testeDiasOK[34:39,]
  diasDezembro <- merge(diasDezembro, diasMes, by.x = "dia_mes_ano", by.y = "dia", all.x = TRUE, all.y = TRUE)
  diasDezembro[is.na(diasDezembro)] <- 0
  
  grupoDez <- melt(data = diasDezembro, id.vars = "dia_mes_ano", measure.vars = c("Predita", "Observada"))
  names(grupoDez)[names(grupoDez) == "variable"] <- "Energia"
  gf_predDez <- ggplot(data = grupoDez , aes(x=dia_mes_ano, y = value, fill = Energia) ) + 
    geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(breaks = 50*0:900, name = "Energia") +
    scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("days"), expand = c(0,0)) +
    theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
    #  theme_ipsum() + 
    ggtitle("Dias - Dezembro/19") +
    theme(axis.text.x = element_text(angle = 60))
  plot(gf_predDez)

  dev.off()

#### DIP, DIO - Mensais (Janeiro/20) ####
      
  nome_arquivo <- paste ("4_Jan20_energia.png")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')
        
  diasMes <- numeric()
  diasMes <- data.frame("dia" = seq(as.Date("2020/1/1"), as.Date("2020/1/31"), "days"))
  diasJaneiro <- testeDiasOK[40:50,]
  diasJaneiro <- merge(diasJaneiro, diasMes, by.x = "dia_mes_ano", by.y = "dia", all.x = TRUE, all.y = TRUE)
  diasJaneiro[is.na(diasJaneiro)] <- 0
        
  grupoJan <- melt(data = diasJaneiro, id.vars = "dia_mes_ano", measure.vars = c("Predita", "Observada"))
  names(grupoJan)[names(grupoJan) == "variable"] <- "Energia"
  gf_predJan <- ggplot(data = grupoJan , aes(x=dia_mes_ano, y = value, fill = Energia) ) + 
  geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(breaks = 50*0:900, name = "Energia") +
    scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("days"), expand = c(0,0)) +
    theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
    #  theme_ipsum() + 
    ggtitle("Dias - Janeiro/20") +
    theme(axis.text.x = element_text(angle = 60))
  plot(gf_predJan)


  
#### DIP, DIO - Mensais (Fevereiro/20) ####
  
  nome_arquivo <- paste ("5_Fev20_energia.png")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')

  diasMes <- numeric()
  diasMes <- data.frame("dia" = seq(as.Date("2020/2/1"), as.Date("2020/2/29"), "days"))
  diasFevereiro <- testeDiasOK[51:66,]
  diasFevereiro <- merge(diasFevereiro, diasMes, by.x = "dia_mes_ano", by.y = "dia", all.x = TRUE, all.y = TRUE)
  diasFevereiro[is.na(diasFevereiro)] <- 0

  grupoFev <- melt(data = diasFevereiro, id.vars = "dia_mes_ano", measure.vars = c("Predita", "Observada"))
  names(grupoFev)[names(grupoFev) == "variable"] <- "Energia"
  gf_predFev <- ggplot(data = grupoFev , aes(x=dia_mes_ano, y = value, fill = Energia) ) + 
    geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(breaks = 50*0:900, name = "Energia") +
    scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("days"), expand = c(0,0)) +
    theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
    #  theme_ipsum() + 
    ggtitle("Dias - Fevereiro/20") +
    theme(axis.text.x = element_text(angle = 60))
  plot(gf_predFev)

  dev.off()


#### DIP, DIO - Mensais (Março/20) ####
  
  nome_arquivo <- paste ("6_Mar20_energia.png")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px') 
  
  
  diasMes <- numeric()
  diasMes <- data.frame("dia" = seq(as.Date("2020/3/1"), as.Date("2020/3/31"), "days"))
  diasMarco <- testeDiasOK[67:88,]
  diasMarco <- merge(diasMarco, diasMes, by.x = "dia_mes_ano", by.y = "dia", all.x = TRUE, all.y = TRUE)
  diasMarco[is.na(diasMarco)] <- 0
  
  grupoMar <- melt(data = diasMarco, id.vars = "dia_mes_ano", measure.vars = c("Predita", "Observada"))
  names(grupoMar)[names(grupoMar) == "variable"] <- "Energia"
  gf_predMar <- ggplot(data = grupoMar , aes(x=dia_mes_ano, y = value, fill = Energia) ) + 
    geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(breaks = 50*0:900, name = "Energia") +
    scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("days"), expand = c(0,0)) +
    theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
    #  theme_ipsum() + 
    ggtitle("Dias - Dezembro/19") +
    theme(axis.text.x = element_text(angle = 60))
  plot(gf_predMar)

  dev.off()

  
  
  
#### DIP, DIO - Mensais (Abr/20) ####
  
  nome_arquivo <- paste ("7_Abr20_energia.png")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px') 
  
  diasMes <- numeric()
  diasMes <- data.frame("dia" = seq(as.Date("2020/04/01"), as.Date("2020/04/30"), "days"))
  diasAbril <- testeDiasOK[89:105,]
  diasAbril <- merge(diasAbril, diasMes, by.x = "dia_mes_ano", by.y = "dia", all.x = TRUE, all.y = TRUE)
  diasAbril[is.na(diasAbril)] <- 0
  
  grupoAbr <- melt(data = diasAbril, id.vars = "dia_mes_ano", measure.vars = c("Predita", "Observada"))
  names(grupoAbr)[names(grupoAbr) == "variable"] <- "Energia"
  gf_predAbr <- ggplot(data = grupoAbr , aes(x=dia_mes_ano, y = value, fill = Energia) ) + 
    geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(breaks = 50*0:900, name = "Energia") +
    scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("days"), expand = c(0,0)) +
    theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
    #  theme_ipsum() + 
    ggtitle("Dias - Dezembro/19") +
    theme(axis.text.x = element_text(angle = 60))
  plot(gf_predAbr)

  
  dev.off()














