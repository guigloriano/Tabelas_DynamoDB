ND1 <- read_ods("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predi��o\\df_SMarco.ods") 


ND1 <- na.omit(ND1)
nr1 <- nrow(ND1)
ir1 <- numeric()
i01 <- 0

for(i in 1:nr1){
  if(ND1[i,6]==0 | ND1[i,3]==0){
    i01 <- i01 + 1
    ir1[i01] <- i
  }
}


if(length(ir1)>0)
  ND1 <- ND1[-ir1,]
n1 <- nrow(ND1)

##### ACUMULADA

id.fi1 <- cumsum(as.numeric(ftable(ND1[,1]))) 
id.in1 <- c(1,id.fi1+1)
AIRR1 <- 0
APDC1 <- 0


for(d in 1:98){
  AIRR1 <- c(AIRR1, cumsum(ND1[id.in1[d]:id.fi1[d],3]))
  APDC1 <- c(APDC1, cumsum(ND1[id.in1[d]:id.fi1[d],6]))
}

NDA1 <- ND1
NDA1[,3] <- AIRR1[-1]     
NDA1[,6] <- APDC1[-1]

##### Transformação

NDA1[,1] <- factor(NDA1[,1])     
NDA1[,2] <- NDA1[,2] - 1
NDA1[,3] <- log(NDA1[,3])
NDA1[,4] <- log(NDA1[,4])
NDA1[,5] <- log(NDA1[,5])
NDA1[,6] <- log(NDA1[,6])

NDA1 <- data.frame(NDA1)

newd1 <- data.frame(NDA1[,1:5])  

pred31 <- as.numeric(predict(M3, newd1))

###### Gráfico por dia

id.fi1 <- cumsum(as.numeric(ftable(NDA1[,1]))) 
id.in1 <- c(1,id.fi1+1)

dia <- 30

##### tempo x PCD

xi1 <- id.in1[dia] 
xf1 <- id.fi1[dia]
Dio1 <- NDA1[xi1:xf1,6]
plot(NDA1[xi1:xf1,2], Dio1, pch=19, lwd=2, ylab="Valores Y", xlab="Tempo", xlim=c(0,length(Dio1)), ylim=c(2,13))
points(NDA1[xi1:xf1,2], Dio1, col="black", type="l", lwd=2)

##### Gráfico tempo x irradiância

d11 <- NDA1[xi1:xf1,2]
d21 <- NDA1[xi1:xf1,3]
d31 <- NDA1[xi1:xf1,5]

View(pred31)
Dip1 <- pred31[xi1:xf1]
points(NDA1[xi1:xf1,2], Dip1, col="red", type="l", lwd=2)

legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)

EQMi1 <- mean((Dio1 - Dip1)^2)
EQMi1 <- round(EQMi1, 4)

teste <- paste("EQM = ", EQMi1)

text(68, 12, teste)
