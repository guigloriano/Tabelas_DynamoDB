library(reshape2)
library(ggplot2)

source("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/cod_1_21dias.R")
source("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/cod_22_42dias.R")
source("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/cod_43_63dias.R")
source("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/cod_64_84dias.R")
source("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/cod_85_105dias.R")



#### criar dataset ####
# x <- data.frame("DIP" = Dip_list, "DIO" = Dio_list)


#### exemplo de impressao ####
sm <- seq(1,length(id.fi)+84)
# dev.off()
p = ggplot() + theme_bw() + 
  geom_line(aes(x = sm, y = Dip_list, group=1), color = "blue", size = 0.5) +
  geom_line(aes(x = sm, y = Dio_list, group=2), color = "red", size = 0.5) +
  geom_point(aes(x = sm, y = Dip_list, group=1), color = "blue", size = 1) +
  geom_point(aes(x = sm, y = Dio_list, group=2), color = "red", size = 1) +
  xlab('Dias') +
  ylab('Potência') + 
  scale_x_continuous(breaks=seq(0, 100, 5))  +
  scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a"))
print(p)


#### inicio da separacao dos DIPs/DIOs ####
#### DIP, DIO - Outubro ###

        nome_arquivo <- paste ("energia2_1outubro_dip.png")
        pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
        fileDest <- paste(pathDest, nome_arquivo, sep = "")
        png(filename = fileDest, width = 720, height = 480, units = 'px')

seq_outDIA <- seq(1,8)
seq_outDip <- Dip_list[1:8]
graf_nov <-barplot(t(as.matrix(seq_outDip)), col=c("gray"), 
                   yaxt = "n", ylab="Energia Predita",xlab="Dia")
axis(2, at = seq(0, 1100, 50), las = 1, cex.axis=0.5)
axis(1, at=graf_out,labels=1:8, cex.axis=0.5)

        dev.off()

        nome_arquivo <- paste ("energia2_1outubro_all.png")
        pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
        fileDest <- paste(pathDest, nome_arquivo, sep = "")
        png(filename = fileDest, width = 720, height = 480, units = 'px')

conjOut <- data.frame("DIA" = seq(1,8), "DIP" = Dip_list[1:8], "DIO" = Dio_list[1:8])
conjOut2 <- melt(data = conjOut, id.vars = "DIA", measure.vars = c("DIP", "DIO"))

gf_out <- ggplot(data = conjOut2 , aes(x=DIA, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energia") +
  scale_x_continuous(breaks = 1*1:8) +
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
#  theme_ipsum() + 
  ggtitle("Dias - Outubro/19") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_out)

        dev.off()




#### DIP, DIO - Novembro ###
        
        nome_arquivo <- paste ("energia2_2novembro_dip.png")
        pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
        fileDest <- paste(pathDest, nome_arquivo, sep = "")
        png(filename = fileDest, width = 720, height = 480, units = 'px')
        
seq_novDIA <- seq(9,33)
seq_novDip <- Dip_list[9:33]
graf_nov <-barplot(t(as.matrix(seq_novDip)), col=c("gray"), 
           yaxt = "n", ylab="Energia Predita",xlab="Dia")
axis(2, at = seq(0, 1100, 50), las = 1, cex.axis=0.5)
axis(1, at=graf_nov,labels=9:33, cex.axis=0.5)

        dev.off()
        
        nome_arquivo <- paste ("energia2_2novembro_all.png")
        pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
        fileDest <- paste(pathDest, nome_arquivo, sep = "")
        png(filename = fileDest, width = 720, height = 480, units = 'px')
        
conjNov <- data.frame("DIA" = seq(9,33), "DIP" = Dip_list[9:33], "DIO" = Dio_list[9:33])
conjNov2 <- melt(data = conjNov, id.vars = "DIA", measure.vars = c("DIP", "DIO"))

gf_nov <- ggplot(data = conjNov2 , aes(x=DIA, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energia") +
  scale_x_continuous(breaks = 1*9:33) +
 theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
  #  theme_ipsum() + 
  ggtitle("Dias - Novembro/19") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_nov)

        dev.off()



#### DIP, DIO - Dezembro ###
        
        nome_arquivo <- paste ("energia2_3dezembro_dip.png")
        pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
        fileDest <- paste(pathDest, nome_arquivo, sep = "")
        png(filename = fileDest, width = 720, height = 480, units = 'px')
        
seq_dezDIA <- seq(34,39)
seq_dezDip <- Dip_list[34:39]
graf_dez <-barplot(t(as.matrix(seq_dezDip)), col=c("gray"), 
                   yaxt = "n", ylab="Energia Predita",xlab="Dia")
axis(2, at = seq(0, 1100, 50), las = 1, cex.axis=0.5)
axis(1, at=graf_dez,labels=34:39, cex.axis=0.5)

      dev.off()
      
      nome_arquivo <- paste ("energia2_3dezembro_all.png")
      pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
      fileDest <- paste(pathDest, nome_arquivo, sep = "")
      png(filename = fileDest, width = 720, height = 480, units = 'px')

conjDez <- data.frame("DIA" = seq(34,39), "DIP" = Dip_list[34:39], "DIO" = Dio_list[34:39])
conjDez2 <- melt(data = conjDez, id.vars = "DIA", measure.vars = c("DIP", "DIO"))

gf_dez <- ggplot(data = conjDez2 , aes(x=DIA, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energia") +
  scale_x_continuous(breaks = 1*34:39) +
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
  #  theme_ipsum() + 
  ggtitle("Dias - Dezembr/19") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_dez)

      dev.off()



#### DIP, DIO - Janeiro ###

      
      nome_arquivo <- paste ("energia2_4janeiro_dip.png")
      pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
      fileDest <- paste(pathDest, nome_arquivo, sep = "")
      png(filename = fileDest, width = 720, height = 480, units = 'px')
      
      
seq_janDIA <- seq(40,50)
seq_janDip <- Dip_list[40:50]
graf_jan <-barplot(t(as.matrix(seq_janDip)), col=c("gray"), 
                   yaxt = "n", ylab="Energia Predita",xlab="Dia")
axis(2, at = seq(0, 1100, 50), las = 1, cex.axis=0.5)
axis(1, at=graf_jan,labels=40:50, cex.axis=0.5)

      dev.off()
      
      
      nome_arquivo <- paste ("energia2_4janeiro_all.png")
      pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
      fileDest <- paste(pathDest, nome_arquivo, sep = "")
      png(filename = fileDest, width = 720, height = 480, units = 'px')
      
conjJan <- data.frame("DIA" = seq(40,50), "DIP" = Dip_list[40:50], 
                      "DIO" = Dio_list[40:50])
conjJan2 <- melt(data = conjJan, id.vars = "DIA", measure.vars = c("DIP", "DIO"))

gf_jan <- ggplot(data = conjJan2 , aes(x=DIA, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energia") +
  scale_x_continuous(breaks = 1*40:50) +
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
  #  theme_ipsum() + 
  ggtitle("Dias - Janeiro/20") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_jan)

      dev.off()



#### DIP, DIO - Fevereiro ###
      
      nome_arquivo <- paste ("energia2_5fevereiro_dip.png")
      pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
      fileDest <- paste(pathDest, nome_arquivo, sep = "")
      png(filename = fileDest, width = 720, height = 480, units = 'px')
      
      
seq_fevDIA <- seq(51,66)
seq_fevDip <- Dip_list[51:66]
graf_fev <-barplot(t(as.matrix(seq_fevDip)), col=c("gray"), 
                   yaxt = "n", ylab="Energia Predita",xlab="Dia")
axis(2, at = seq(0, 1100, 50), las = 1, cex.axis=0.5)
axis(1, at=graf_fev,labels=51:66, cex.axis=0.5)

      dev.off()
      nome_arquivo <- paste ("energia2_5fevereiro_all.png")
      pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
      fileDest <- paste(pathDest, nome_arquivo, sep = "")
      png(filename = fileDest, width = 720, height = 480, units = 'px')

conjFev <- data.frame("DIA" = seq(51,66), "DIP" = Dip_list[51:66], 
                      "DIO" = Dio_list[51:66])
conjFev2 <- melt(data = conjFev, id.vars = "DIA", measure.vars = c("DIP", "DIO"))

gf_fev <- ggplot(data = conjFev2 , aes(x=DIA, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energia") +
  scale_x_continuous(breaks = 1*51:66) +
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
  #  theme_ipsum() + 
  ggtitle("Dias - Fevereiro/20") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_fev)
  
    dev.off()


#### DIP, DIO - MARCO ###

    
    nome_arquivo <- paste ("energia2_6marco_dip.png")
    pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
    fileDest <- paste(pathDest, nome_arquivo, sep = "")
    png(filename = fileDest, width = 720, height = 480, units = 'px')
    
    
seq_marDIA <- seq(67,88)
seq_marDip <- Dip_list[67:88]
graf_mar <-barplot(t(as.matrix(seq_marDip)), col=c("gray"), 
                   yaxt = "n", ylab="Energia Predita",xlab="Dia")
axis(2, at = seq(0, 1100, 50), las = 1, cex.axis=0.5)
axis(1, at=graf_mar,labels=67:88, cex.axis=0.5)

      dev.off()
      nome_arquivo <- paste ("energia2_6marco_all.png")
      pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
      fileDest <- paste(pathDest, nome_arquivo, sep = "")
      png(filename = fileDest, width = 720, height = 480, units = 'px')

      
      
conjmar <- data.frame("DIA" = seq(67,88), "DIP" = Dip_list[67:88], 
                      "DIO" = Dio_list[67:88])
conjmar2 <- melt(data = conjmar, id.vars = "DIA", measure.vars = c("DIP", "DIO"))

gf_mar <- ggplot(data = conjmar2 , aes(x=DIA, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(breaks = 50*0:900, name = "Energia") +
  scale_x_continuous(breaks = 1*67:88) +
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
  #  theme_ipsum() + 
  ggtitle("Dias - Março/20") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_mar)
        dev.off()

#### DIP, DIO - ABRIL ###
        
        
        nome_arquivo <- paste ("energia2_7abril_dip.png")
        pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
        fileDest <- paste(pathDest, nome_arquivo, sep = "")
        png(filename = fileDest, width = 720, height = 480, units = 'px')
        
seq_abrDIA <- NULL
seq_abrDIA <- seq(89,105)
seq_abrDip <- Dip_list[89:105]

graf_abr <-barplot(t(as.matrix(seq_abrDip)), col=c("gray"), 
                   yaxt = "n", ylab="Energia Predita",xlab="Dia")
axis(2, at = seq(0, 1100, 50), las = 1, cex.axis=0.5)
axis(1, at=graf_abr,labels=89:105, cex.axis=0.5)

      dev.off()
      nome_arquivo <- paste ("energia2_7abril_all.png")
      pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
      fileDest <- paste(pathDest, nome_arquivo, sep = "")
      png(filename = fileDest, width = 720, height = 480, units = 'px')
      
conjabr <- data.frame("DIA" = seq_abrDIA, "DIP" = Dip_list[89:105], 
                      "DIO" = Dio_list[89:105])
conjabr2 <- melt(data = conjabr, id.vars = "DIA", measure.vars = c("DIP", "DIO"))

gf_abr <- ggplot(data = conjabr2 , aes(x=DIA, y = value, fill = variable) ) + 
  geom_bar(stat="identity", position="dodge") +
  geom_line()
  scale_y_continuous(breaks = 50*0:900, name = "Energia") +
  scale_x_continuous(breaks = 1*89:105) +
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
  #  theme_ipsum() + 
  ggtitle("Dias - Março/20") +
  theme(axis.text.x = element_text(angle = 60))
plot(gf_abr)

      dev.off()

