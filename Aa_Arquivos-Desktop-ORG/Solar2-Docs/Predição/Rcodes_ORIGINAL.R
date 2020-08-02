###################################################################
########## Análise de dados - Ricardo #############################
###################################################################

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
     
###################################################################
##### Leitura dos dados ###########################################
###################################################################

        #dados <- read_ods("/Users/erlandison/Desktop/Dados_Ricardo/Dados/df_Outubro19.ods") 
         dados <- read_ods("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predi��o\\df_Outubro19.ods")
           
           str(dados)
      
            D <- data.frame(dados)

            n <- nrow(D)
            
##### ACUMULADA
            
        id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
        id.in <- c(1,id.fi+1)
         AIRR <- 0
         APDC <- 0
for(d in 1:21){
         AIRR <- c(AIRR, cumsum(dados[id.in[d]:id.fi[d],3]))
         APDC <- c(APDC, cumsum(dados[id.in[d]:id.fi[d],6]))
              }
            
           DA <- D
       DA[,3] <- AIRR[-1]     
       DA[,6] <- APDC[-1]
            
##### Transformação
            
       DA[,1] <- factor(DA[,1])     
       DA[,2] <- DA[,2] - 1
       DA[,3] <- log(DA[,3])
       DA[,4] <- log(DA[,4])
       DA[,5] <- log(DA[,5])
       DA[,6] <- log(DA[,6])
            
           DA <- data.frame(DA)
            
##############################
##### Modelo M1 - Logístico 1
##############################
               
       f.logi <- function(x1, alpha1, beta0, beta1)
              {
              (alpha1) - log(1 + exp(beta0 - beta1*x1))
              } 
       
           v0 <- c(alpha1=1, beta0=1, beta1=0.1) 
           M1 <- nlme(PDC ~ f.logi(TIME, alpha1, beta0, beta1), 
                      data = DA, 
                      fixed=(alpha1 + beta0 + beta1 ~ 1),
                      start=v0, groups = ~DIA)
           
       summary(M1)
       
        pred1 <- as.numeric(predict(M1))

           AIC(M1)
           BIC(M1)
    
##############################
##### Modelo M2 - Logístico 2
##############################
          
           v0 <- v0 <- c(alpha1=1, beta0=0.1,1,1, beta1=1)  
           M2 <- update(M1, fixed=c(alpha1~1, beta0 ~ IRR + MA, beta1 ~ 1), random=(beta0~1), start=v0)
       summary(M2) 
        pred2 <- as.numeric(predict(M2))
          
           AIC(M2)
           BIC(M2)
           
##############################
##### Modelo M3 - Logístico 3
##############################
          
           v0 <- v0 <- c(alpha1=1, beta0=0.1,1,1, beta1=1)  
           M3 <- update(M1, fixed=c(alpha1~1, beta0 ~ IRR + MA, beta1 ~ 1), random=(beta0~1), start=v0, correlation = corAR1())
       summary(M3)
        
        pred3 <- predict(M3)
        
           AIC(M3)
           BIC(M3)
           
############################
##### Modelo M4 - Gompertz 1
############################
        
       f.gomp <- function(x1, alpha1, beta0, beta1)
              {
              (alpha1) - exp(beta0 - beta1*x1)
              } 
        
           v0 <- c(alpha1=1, beta0=1, beta1=0.1)  
           M4 <- nlme(PDC ~ f.gomp(TIME, alpha1, beta0, beta1), 
                 data = DA, 
                 fixed=(alpha1 + beta0 + beta1 ~ 1),
                 start=v0, groups = ~DIA)
          
       summary(M4)
        
        pred4 <- as.numeric(predict(M4))
        
           AIC(M4)
           BIC(M4)
        
##############################
##### Modelo M5 - Gompertz 2
##############################
           
           v0 <- c(alpha1=1, beta0=0.1,1,1, beta1=1)  
           M5 <- update(M4, fixed=c(alpha1~1, beta0 ~ IRR + MA, beta1 ~ 1), random=(beta0~1), start=v0)
           
       summary(M5)
           
        pred5 <- as.numeric(predict(M5))
           
           AIC(M5)
           BIC(M5)
           
##############################
##### Modelo M6 - Gompertz 3
##############################
           
           v0 <- c(alpha1=1, beta0=0.1,1,1, beta1=1)  
           M6 <- update(M4, fixed=c(alpha1~1, beta0 ~ IRR + MA, beta1 ~ 1), random=(beta0~1), start=v0, correlation = corCAR1())
           
       summary(M6)
           
        pred6 <- as.numeric(predict(M6))
           
           AIC(M6)
           BIC(M6)
           
#######################                
##### Graficos por dia    
#######################
           
        id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
        id.in <- c(1,id.fi+1)
         
          dia <- 1
         
##### tempo x PCD
         
           xi <- id.in[dia] 
           xf <- id.fi[dia]
          Dio <- DA[xi:xf,6]
          plot(DA[xi:xf,2], Dio, type="l", lwd=2, ylab="Valores Y", xlab="Tempo", xlim=c(0,length(Dio)), col="black")
        points(DA[xi:xf,2], Dio, pch=1, lwd=2, col="black")
         
##### Gráfico tempo x irradiância
         
           d1 <- DA[xi:xf,2]
           d2 <- DA[xi:xf,3]
           d3 <- DA[xi,xf,5]
          Dip <- as.numeric(pred3[xi:xf])
        points(DA[xi:xf,2], Dip, col="red", type="l", lwd=2)
           ni <- length(Dip)
         
        legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)
    
##### Erro Qudratico médio do dia
         
          EQM <- mean((Dio - Dip)^2)
          EQM 
      
##############################################      
##### Diferença Percentual média #############
##############################################      

        id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
        id.in <- c(1,id.fi+1)
          Dim <- numeric()

for(i in 1:21){
          dia <- i
           xi <- id.in[dia] 
           xf <- id.fi[dia]
          Dio <- DA[xi:xf,6]
          Dip <- as.numeric(pred3[xi:xf])
       Dim[i] <- round(mean(((Dio - Dip)/Dio)*100), 4)
              } 

         round(Dim,2)   

##### Gráfico em barras das diferenças percentuais médias
         
          dia <- seq(1,21) 
           par(mai=c(1,1,0.3,0.3))
       barplot(Dim, names.arg = dia, cex.names=0.6, ylim=c(-1.5,0.5), ylab="Diferença Percentual Média",xlab="Dia") 
         
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
         
############################
##### Modelo SELECIONADO M3         
############################
        
##### PROJEÇÕES
      
      #     ND <- read_ods("/Users/erlandison/Desktop/Dados_Ricardo/Dados/df_Marco20.ods") 
           ND <- read_ods("C:\\Users\\Guilherme\\Desktop\\Solar2-Docs\\Predi��o\\df_Marco20.ods")
           nr <- nrow(ND)
           ir <- numeric()
           i0 <- 0
for(i in 1:nr){
            if(ND[i,6]==0 | ND[i,3]==0)
              {
           i0 <- i0 + 1
       ir[i0] <- i
              }
              }
        
            if(length(ir)>0)
           ND <- ND[-ir,]
           
            n <- nrow(ND)
           
##### ACUMULADA
           
        id.fi <- cumsum(as.numeric(ftable(ND[,1]))) 
        id.in <- c(1,id.fi+1)
         AIRR <- 0
         APDC <- 0
for(d in 1:21){
         AIRR <- c(AIRR, cumsum(ND[id.in[d]:id.fi[d],3]))
         APDC <- c(APDC, cumsum(ND[id.in[d]:id.fi[d],6]))
              }
           
          NDA <- ND
      NDA[,3] <- AIRR[-1]     
      NDA[,6] <- APDC[-1]
           
##### Transformação
           
      NDA[,1] <- factor(NDA[,1])     
      NDA[,2] <- NDA[,2] - 1
      NDA[,3] <- log(NDA[,3])
      NDA[,4] <- log(NDA[,4])
      NDA[,5] <- log(NDA[,5])
      NDA[,6] <- log(NDA[,6])
           
          NDA <- data.frame(NDA)
          
         newd <- data.frame(NDA[,1:5])  
           
        pred3 <- as.numeric(predict(M3, newd))
            
###### Gráfico por dia
          
        id.fi <- cumsum(as.numeric(ftable(NDA[,1]))) 
        id.in <- c(1,id.fi+1)
          
          dia <- 1
          
##### tempo x PCD
          
           xi <- id.in[dia] 
           xf <- id.fi[dia]
          Dio <- NDA[xi:xf,6]
          plot(NDA[xi:xf,2], Dio, pch=19, lwd=2, ylab="Valores Y", xlab="Tempo", xlim=c(0,length(Di)), ylim=c(2,13))
        points(NDA[xi:xf,2], Dio, col="black", type="l", lwd=2)
          
##### Gráfico tempo x irradiância
          
           d1 <- NDA[xi:xf,2]
           d2 <- NDA[xi:xf,3]
           d3 <- NDA[xi,xf,5]
          Dip <- pred3[xi:xf]
        points(NDA[xi:xf,2], Dip, col="red", type="l", lwd=2)
          
        legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)
        
         EQMi <- mean((Dio - Dip)^2)
         EQMi
         
         text(68, 12, "EQM=0.0067")
         
         text(68, 12, "EQM=0.0036")
         
         text(68, 11, "EQM=0.0128")
         
         text(68, 11, "EQM=0.0047")
         
##### Erro quadrático médio por dia
          
          eqm <- numeric()
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
      
           xa <- seq(1,21, 0.1)
           ya <- rep(0.0083, length(xa))
         lines(xa, ya, lty=2)
    
##########           
##########
##########          
      
for(i in 1:21){
          dia <- i
           xi <- id.in[dia] 
           xf <- id.fi[dia]
          Dio <- NDA[xi:xf,6]
          Dip <- as.numeric(pred3[xi:xf])
       Dim[i] <- round(mean(((Dio - Dip)/Dio)*100), 4)
              } 
    
         round(Dim,2)   
    
##### Gráfico em barras das diferenças percentuais médias
    
          dia <- seq(1,21) 
           par(mai=c(1,1,0.3,0.3))
       barplot(Dim, names.arg = dia, cex.names=0.6, ylim=c(-1.1,0.1), ylab="Diferença Percentual Média",xlab="Dia") 
    
       text(0.7, -0.45, "-0.42", cex=0.5)
       text(1.9, -0.23, "-0.20", cex=0.5)
       text(3.1, -0.39, "-0.36", cex=0.5)
       text(4.3, -0.45, "-0.42", cex=0.5)
       text(5.5, -0.53, "-0.50", cex=0.5)
       text(6.7, -0.62, "-0.59", cex=0.5)
       text(7.9, -0.49, "-0.46", cex=0.5)
       text(9.1, -0.43, "-0.40", cex=0.5)
       text(10.3,-0.48, "-0.45", cex=0.5)
       text(11.5,-0.63, "-0.60", cex=0.5)
       text(12.7,-0.81, "-0.78", cex=0.5)
       text(13.9,-0.77, "-0.74", cex=0.5)
       text(15.1,-0.66, "-0.63", cex=0.5)
       text(16.3,-0.69, "-0.66", cex=0.5)
       text(17.5,-1.08, "-1.05", cex=0.5)
       text(18.7,-0.93, "-0.90", cex=0.5)
       text(19.9,-0.85, "-0.82", cex=0.5)
       text(21.1,-0.56, "-0.53", cex=0.5)
       text(22.3,-0.68, "-0.65", cex=0.5)
       text(23.5,-0.53, "-0.50", cex=0.5)
       text(24.7, 0.05, "0.02", cex=0.5)
    
     