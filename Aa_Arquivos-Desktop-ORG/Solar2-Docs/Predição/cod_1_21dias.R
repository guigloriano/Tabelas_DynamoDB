
############ Cabeçalho #############
rm(list=ls(all=TRUE))
set.seed(19)
library(compiler)
enableJIT(3)

library(lme4)
library(tidyverse)  
library(gdata)
library(nlme)
library(RVAideMemoire)
library(readODS)
library(car)
require(MASS)
library(predictmeans)
library(ggplot2)
library(ggpubr)
library(nlraa)


########### Leitura dos dados #################

dados <- read_ods("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/df_Outubro19.ods") 

str(dados)
D <- data.frame(dados)
n <- nrow(D)

##### ACUMULADA ##### 

id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)
AIRR <- 0
APDC <- 0


for(d in 1:length(id.fi)){
  AIRR <- c(AIRR, cumsum(dados[id.in[d]:id.fi[d],3]))
  APDC <- c(APDC, cumsum(dados[id.in[d]:id.fi[d],6]))
}

DA <- D
DA[,3] <- AIRR[-1]     
DA[,6] <- APDC[-1]

##### Transformação Logartimica ##### 

DA[,1] <- factor(DA[,1])     
DA[,2] <- DA[,2] - 1
DA[,3] <- log(DA[,3])
DA[,4] <- log(DA[,4])
DA[,5] <- log(DA[,5])
DA[,6] <- log(DA[,6])
DA <- data.frame(DA)



##### Modelo M1 - Logístico 1 ##### 

f.logi <- function(x1, alpha1, beta0, beta1)
{
  (alpha1) - log(1 + exp(beta0 - beta1*x1))
} 

v0 <- c(alpha1=1, beta0=1, beta1=0.1) 

#DA_aux <- DA
#DA_aux$DIA <- 1
M1 <- nlme(PDC ~ f.logi(TIME, alpha1, beta0, beta1), 
           data = DA, 
           fixed=(alpha1 + beta0 + beta1 ~ 1),
           start=v0, groups = ~DIA)

summary(M1)
pred1 <- as.numeric(predict(M1))

AIC(M1)
BIC(M1)




##### Modelo M3 - Logístico 3 #####

v0 <- v0 <- c(alpha1=1, beta0=0.1,1,1, beta1=1)  
M3 <- update(M1, fixed=c(alpha1~1, beta0 ~ IRR + MA, beta1 ~ 1), random=(beta0~1), start=v0, correlation = corAR1())

summary(M3)
pred3 <- predict(M3)

AIC(M3)
BIC(M3)



##### Graficos por dia  #####    

id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)
dia <- 1

##### tempo x PCD ##### 

xi <- id.in[dia] 
xf <- id.fi[dia]
Dio <- DA[xi:xf,6]
plot(DA[xi:xf,2], Dio, type="l", lwd=2, ylab="Valores Y", xlab="Tempo", xlim=c(0,length(Dio)), col="black")
points(DA[xi:xf,2], Dio, pch=1, lwd=2, col="black")



##### tempo x irradiancia ##### 

d1 <- DA[xi:xf,2]
d2 <- DA[xi:xf,3]
d3 <- DA[xi:xf,5]
Dip <- as.numeric(pred3[xi:xf])
points(DA[xi:xf,2], Dip, col="red", type="l", lwd=2)
ni <- length(Dip)

legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)





##### Erro Qudratico médio do dia ##### 

EQM <- mean((Dio - Dip)^2)
EQM 




##### Diferença Percentual média #########   

id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)
Dim <- numeric()


for(i in 1:length(id.fi)){
  dia <- i
  xi <- id.in[dia] 
  xf <- id.fi[dia]
  Dio <- DA[xi:xf,6]
  Dip <- as.numeric(pred3[xi:xf])
  Dim[i] <- round(mean(((Dio - Dip)/Dio)*100), 4)
} 

round(Dim,2)   

##### Gráfico em barras das diferenças percentuais médias ##### 

dia <- seq(1,length(id.fi)) 
par(mai=c(1,1,0.3,0.3))
barplot(Dim, names.arg = dia, cex.names=0.6, ylim=c(-1.6,0.5), ylab="DiferenÃ§a Percentual MÃ©dia",xlab="Dia") 

text(0.7, -0.21, "-0.18", cex=0.5)
text(1.9, -0.50, "-0.47", cex=0.5)
text(3.1, -0.34, "-0.31", cex=0.5)
text(4.3, -0.37, "-0.34", cex=0.5)
text(5.5, -0.25, "-0.22", cex=0.5)
text(6.7, -0.31, "-0.28", cex=0.5)
text(7.9, -0.37, "-0.34", cex=0.5)
text(9.1, -0.30, "-0.27", cex=0.5)
text(10.3,-1.20, "-1.17", cex=0.5)
text(11.5,-0.88, "-0.85", cex=0.5)
text(12.7,-1.34, "-1.31", cex=0.5)
text(13.9,-0.25, "-0.22", cex=0.5)
text(15.1,-0.23, "-0.20", cex=0.5)
text(16.3,0.14, "0.11", cex=0.5)
text(17.5,-0.35, "-0.32", cex=0.5)
text(18.7,-0.44, "0.40", cex=0.5)
text(19.9,-0.38, "-0.35", cex=0.5)
text(21.1,-0.40, "-0.37", cex=0.5)
text(22.3,0.12, "0.08", cex=0.5)
text(23.5,-0.54, "-0.51", cex=0.5)
text(24.7,-0.20, "-0.17", cex=0.5)

##### Modelo SELECIONADO M3 #####        

##### PROJEÇÕES ####

#ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/df_Marco20.ods") 
#ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/df_SOutubro.ods") 
#ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\csv_dia\\df_SOutubro.ods") 

ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\1_21.ods")
#ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\63.ods")
#ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\84.ods")
#ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\105.ods")


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
xi <- numeric()
xf <- numeric()
Dip_list <- numeric()
Dio_list <- numeric()
EQMi <- numeric()

##### tempo x PCD #####

for(i in 1:length(id.fi)){
  
  nome_arquivo <- paste ("dia_", i, ".png", sep="")
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
  Dip_list[i] <- sum(Dip)
  Dio_list[i] <- sum(Dio)
  
  EQMi[i] <- mean((Dio - Dip)^2)
  #  EQMi[i] 
  
  EQMi[i] <- round(EQMi[i], 4)
  textoEQM <- paste("EQM = ", EQMi[i])
  text(68, 11, textoEQM)
  dev.off()  
  
}


#### Impressao de DIP/DIO para todos os dias ####

nome_arquivo <- paste ("energia_", i, ".png", sep="")

pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
fileDest <- paste(pathDest, nome_arquivo, sep = "")

png(filename = fileDest, width = 720, height = 480, units = 'px')


sm <- seq(1,length(id.fi))
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

eqm <- numeric()
for(i in 1:(length(id.fi))){         
  dia <- i
  xi <- id.in[dia] 
  xf <- id.fi[dia]
  Dio <- NDA[xi:xf,6]
  Dip <- pred3[xi:xf]
  eqm[i] <- mean((Dio - Dip)^2)
}

eqm2 <- na.omit(eqm)
#View(eqm2)
med_imp <- mean(eqm2)

  nome_arquivo <- paste ("eqm1_", i, ".png", sep="")

  pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
  fileDest <- paste(pathDest, nome_arquivo, sep = "")

  png(filename = fileDest, width = 720, height = 480, units = 'px')



sm <- seq(1,length(id.fi))
plot(sm, eqm2, type="l", xlim=c(0,21),  ylim=c(0,0.03), ylab="EQM", xlab="Dia")
points(sm, eqm2, pch=19)


  dev.off()
#xa <- seq(1, 97, 0.1)
#ya <- rep(med_imp, length(xa))
#lines(xa, ya, lty=2)

##########           
##########
##########          

i <- 1 
Dim <- numeric()
for(i in 1:length(id.fi)){
  dia <- i
  xi <- id.in[dia] 
  xf <- id.fi[dia]
  Dio <- NDA[xi:xf,6]
  Dip <- as.numeric(pred3[xi:xf])
  Dim[i] <- round(mean(((Dio - Dip)/Dio)*100), 4)
} 

round(Dim,2)   

  nome_arquivo <- paste ("dif_perc1_", i, ".png", sep="")

  pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
  fileDest <- paste(pathDest, nome_arquivo, sep = "")

  png(filename = fileDest, width = 720, height = 480, units = 'px')



##### GrÃ¡fico em barras das diferenÃ§as percentuais mÃ©dias

dia <- seq(1,length(id.fi)) 
par(mai=c(1,1,0.3,0.3))
barplot(Dim, names.arg = dia, cex.names=0.6, ylim=c(-1.5,1), 
        ylab="Diferenca Percentual Media",xlab="Dia") 

dev.off()
# View(Dim)

source("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/cod_22_42dias.R")
source("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/cod_43_63dias.R")
source("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/cod_64_84dias.R")
source("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/cod_85_105dias.R")

