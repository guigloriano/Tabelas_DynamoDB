dia <- 1

for (dia in 1:21){
  ##### tempo x PCD
  
  png(filename = fileDest, width = 1550, height = 720, units = 'px')
  xi <- id.in[dia] 
  xf <- id.fi[dia]
  Dio <- NDA[xi:xf,6]
  plot(NDA[xi:xf,2], Dio, pch=19, lwd=2, ylab="Valores Y", xlab="Tempo", xlim=c(0,length(Dio)), ylim=c(2,13))
  points(NDA[xi:xf,2], Dio, col="black", type="l", lwd=2)
  
  ##### Gráfico tempo x irradiância
  
  d1 <- NDA[xi:xf,2]
  d2 <- NDA[xi:xf,3]
  d3 <- NDA[xi:xf,5]
  Dip <- pred3[xi:xf]
  points(NDA[xi:xf,2], Dip, col="red", type="l", lwd=2)
  
  legend("bottomright", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)
  
  EQMi <- mean((Dio - Dip)^2)
  EQMi
  
  EQMi <- round(EQMi, 4)
  teste <- paste("EQM = ", EQMi)
  
  
  text(68, 12, teste)
  
  nome_arquivo <- paste ("eqm_", dia+84, ".png", sep="")
  
  pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\teste\\"
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  #  png(filename = fileDest, width = 1280, height = 720, units = 'px')
#  plot(gf_1prim)
  dev.off()  
  
  ##### Erro quadrático médio por dia
  
#  eqm <- numeric()
  
  
#  NDA2 <- NDA
#  NDA <- NDA
  
  
  
for(i in 1:21){  
    dia <- i
    xi <- id.in[dia] 
    xf <- id.fi[dia]
    Dio <- NDA[xi:xf,6]
    Dip <- pred3[xi:xf]
    eqm[i] <- mean((Dio - Dip)^2)
}
  
eqm
mean(eqm)
  
sm <- seq(1,21)
plot(sm, eqm, type="l", ylim=c(0,0.016), ylab="EQM", xlab="Dia")
points(sm, eqm, pch=19)
  
#  xa <- seq(1,21, 0.1)
#  ya <- rep(0.0083, length(xa))
#  lines(xa, ya, lty=2)
  
}

