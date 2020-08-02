ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\43_63.ods")


nr <- nrow(ND)
ir <- numeric()
i0 <- 0

# encontra as linhas que IRR e PDC sao zeros ####
for(i in 1:nr){
  if(ND[i,6]==0 | ND[i,3]==0)
  {
    i0 <- i0 + 1
    ir[i0] <- i
  }
}

# remove as linhas que a IRR ou PDC sao zeros ####
if(length(ir)>0){
  ND <- ND[-ir,]
}

n <- nrow(ND)

##### ACUMULADA  ####

id.fi <- cumsum(as.numeric(ftable(ND[,1]))) 
id.in <- c(1,id.fi+1)

AIRR <- 0
APDC <- 0

for(d in 1:length(id.fi)){
  AIRR <- c(AIRR, cumsum(ND[id.in[d]:id.fi[d],3]))
  APDC <- c(APDC, cumsum(ND[id.in[d]:id.fi[d],6]))
}

# View(AIRR)

NDA <- ND
NDA[,3] <- AIRR[-1]     
NDA[,6] <- APDC[-1]

##### Transformação ####

NDA[,1] <- factor(NDA[,1])     
NDA[,2] <- NDA[,2] - 1
NDA[,3] <- log(NDA[,3])
NDA[,4] <- log(NDA[,4])
NDA[,5] <- log(NDA[,5])
NDA[,6] <- log(NDA[,6])
NDA <- data.frame(NDA)

newd <- data.frame(NDA[,1:5])  
newd <- na.exclude(newd)

pred3 <- as.numeric(predict(M3, newd))

# View(pred3)
# summary(M3)

###### Gráfico por dia ####

id.fi <- cumsum(as.numeric(ftable(NDA[,1]))) 
id.in <- c(1,id.fi+1)

#dia <- 97
#i <- 1
#xi <- numeric()
#xf <- numeric()
#Dip_list <- numeric()
#Dio_list <- numeric()
#EQMi <- numeric()

##### tempo x PCD #####

for(i in 1:length(id.fi)){
  
  nome_arquivo <- paste ("dia_", i+42, ".png", sep="")
  pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')
  
  dia <- i
  xi[dia] <- id.in[dia] 
  xf[dia] <- id.fi[dia]
  Dio <- NDA[xi[dia]:xf[dia],6]
  plot(NDA[xi[dia]:xf[dia],2], Dio, pch=19, lwd=2, ylab="Valores Y", xlab="Tempo", xlim=c(0,length(Dio)), ylim=c(2,13))
  points(NDA[xi[dia]:xf[dia],2], Dio, col="black", type="l", lwd=2)
  
  
  ##### tempo x irradiancia ####
  
  d1 <- NDA[xi[dia]:xf[dia],2]
  d2 <- NDA[xi[dia]:xf[dia],3]
  d3 <- NDA[xi[dia]:xf[dia],5]
  Dip <- pred3[xi[dia]:xf[dia]]
  points(NDA[xi[dia]:xf[dia],2], Dip, col="red", type="l", lwd=2)
  
  legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)
  
  #  View(Dip_list)
  Dip_list[i+42] <- sum(Dip)
  Dio_list[i+42] <- sum(Dio)
  
  EQMi[i+42] <- mean((Dio - Dip)^2)
  #  EQMi[i] 
  
  EQMi[i+42] <- round(EQMi[i+42], 4)
  textoEQM <- paste("EQM = ", EQMi[i+42])
  text(68, 11, textoEQM)
  dev.off()  
  
}


#### Impressao de DIP/DIO para todos os dias ####
nome_arquivo <- paste ("energia_", i+42, ".png", sep="")

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
fileDest <- paste(pathDest, nome_arquivo, sep = "")

png(filename = fileDest, width = 720, height = 480, units = 'px')

sm <- seq(1,length(id.fi)+42)
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
dev.off()


##### Erro quadrático médio por dia #####

#eqm <- numeric()
for(i in 1:(length(id.fi))){         
  dia <- i
  xi <- id.in[dia] 
  xf <- id.fi[dia]
  Dio <- NDA[xi:xf,6]
  Dip <- pred3[xi:xf]
  eqm[i+42] <- mean((Dio - Dip)^2)
}

eqm2 <- na.omit(eqm)
#View(eqm2)
med_imp <- mean(eqm2)

nome_arquivo <- paste ("eqm1_", i+42, ".png", sep="")

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
fileDest <- paste(pathDest, nome_arquivo, sep = "")

png(filename = fileDest, width = 720, height = 480, units = 'px')



sm <- seq(1,length(id.fi)+42)
plot(sm, eqm2, type="l", xlim=c(0,64),  ylim=c(0,0.03), ylab="EQM", xlab="Dia")
points(sm, eqm2, pch=19)


dev.off()
#xa <- seq(1, 97, 0.1)
#ya <- rep(med_imp, length(xa))
#lines(xa, ya, lty=2)

##########           
##########
##########          

#i <- 1 
#Dim <- numeric()
for(i in 1:length(id.fi)){
  dia <- i
  xi <- id.in[dia] 
  xf <- id.fi[dia]
  Dio <- NDA[xi:xf,6]
  Dip <- as.numeric(pred3[xi:xf])
  Dim[i+42] <- round(mean(((Dio - Dip)/Dio)*100), 4)
} 

#round(Dim,3)   

nome_arquivo <- paste ("dif_perc1_", i+42, ".png", sep="")

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
fileDest <- paste(pathDest, nome_arquivo, sep = "")

png(filename = fileDest, width = 720, height = 480, units = 'px')



##### GrÃ¡fico em barras das diferenÃ§as percentuais mÃ©dias

dia <- seq(1,length(id.fi)+42) 
par(mai=c(1,1,0.3,0.3))
barplot(Dim, names.arg = dia, cex.names=0.6, ylim=c(-1.5,1), ylab="Diferenca Percentual Media",xlab="Dia") 

dev.off()
# View(Dim)


