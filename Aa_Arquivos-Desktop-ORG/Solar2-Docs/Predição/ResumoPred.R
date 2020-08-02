
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
#dados1 <- read_ods("C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\csv_dia\\df_Outubro19.ods") 

str(dados)
D <- data.frame(dados)
n <- nrow(D)

# ir <- numeric()
# i0 <- 0
# # encontra as linhas que IRR e PDC sao zeros
# for(i in 1:n){
#   if(dados1[i,6]==0 | dados1[i,3]==0){
#     i0 <- i0 + 1
#     ir[i0] <- i
#   }
# }
# 
# # remove as linhas que a IRR ou PDC sao zeros
# if(length(ir)>0){
#   dados1 <- dados1[-ir,]
# }


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

DA_aux <- DA
DA_aux$DIA <- 1
M1 <- nlme(PDC ~ f.logi(TIME, alpha1, beta0, beta1), 
           data = DA_aux, 
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
barplot(Dim, names.arg = dia, cex.names=0.6, ylim=c(-1.0,0.6), ylab="Diferença Percentual Média",xlab="Dia") 


##### Modelo SELECIONADO M3 #####        

##### PROJEÇÕES ####

#ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/df_Marco20.ods") 
#ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predição/df_SOutubro.ods") 
ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\csv_dia\\df_SOutubro.ods") 

#ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\42.ods")
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
  
  if (ND$DIA[i] > 12){
    ND$DIA[i] <- ND$DIA[i] - 23
    
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

newd_aux <- newd
newd_aux$DIA <- 1

pred3 <- as.numeric(predict(M3, newd_aux))

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

##### tempo x PCD #####
EQMi <- numeric()

for(i in 1:(length(id.fi)-1)){
  
  nome_arquivo <- paste ("teste_", i, ".png", sep="")
  pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\teste3\\"
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
sm <- seq(1,length(id.fi)-1)
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
library(reshape2)

##### Erro quadrático médio por dia #####

eqm <- numeric()
for(i in 1:(length(id.fi)-1)){         
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

# nome_arquivo <- paste ("teste2_", i, ".png", sep="")
# 
# pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\teste2\\"
# fileDest <- paste(pathDest, nome_arquivo, sep = "")
# 
# png(filename = fileDest, width = 720, height = 480, units = 'px')
# 
# dev.off()


sm <- seq(1,length(id.fi)-1)
plot(sm, eqm2, type="l", xlim=c(0,100),  ylim=c(0,0.09), ylab="EQM", xlab="Dia")
points(sm, eqm2, pch=19)



xa <- seq(1, 97, 0.1)
ya <- rep(med_imp, length(xa))
lines(xa, ya, lty=2)

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

##### GrÃ¡fico em barras das diferenÃ§as percentuais mÃ©dias

 dia <- seq(1,length(id.fi)) 
 par(mai=c(1,1,0.3,0.3))
 barplot(Dim, names.arg = dia, cex.names=0.6, ylim=c(-1.0,2.5), ylab="DiferenÃ§a Percentual MÃ©dia",xlab="Dia") 

# View(Dim)


