
aux_dia <- NULL
df <- NULL 
RL_final <- data.frame()

for(i in 1:length(names)){   
  
 # i = 2
  
  # leitura do dataset com o nome na i-nesima posicao da pasta
  dataReg <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))   
  
  # aux para pegar o dia do dataset
  aux_dia[i] <- dataReg$dia_mes_ano[1]
  
  # df para acumular os datasets lidos em um unico arquivo
  df <- rbind(df, dataReg)
  # View(df)
  
  #------------------------------------------------------------------------------------
  # comando para transformar NaN's e Na's em 0
  df[is.na(df)] <- 0
  
  # comando para remover os NaN's e Na's dos datasets
  # dataReg <- na.omit(dataReg)
  #------------------------------------------------------------------------------------
  
  # definicao do modelo e calculo da regressao linear usando suas variaveis
  modelo <- df$P_AC ~ df$irr_est + df$temp + df$numPM1 + df$massaPM1 + df$numPM2 + df$massaPM2
  reg_linear <- lm(modelo, data = df)
  # summary(reg_linear)
  
  #------------------------------------------------------------------------------------
  # coefficients(reg_linear)      # exibe os coeficientes encontrados pela Reg. Linear
  # residuals(reg_linear)         # exibe os residuais encontrados pela Reg. Linear
  
   # Calcula os residuais individuaalmente
   Res_Min <- min(residuals(reg_linear))                # minimo
   Res_1Q <- quantile(residuals(reg_linear), 0.25)     # 1o. Quantil
   Res_Mdn <- median(residuals(reg_linear))             # mediana
   Res_3Q <- quantile(residuals(reg_linear), 0.75)     # 3o. Quantil
   Res_Max <- max(residuals(reg_linear))                # maximo
  
  # Calcula os residuais (min, 1Q, MEDIAN, 3Q, max) de uma vez
  # quantile(residuals(reg_linear), probs= c(0, 0.25, 0.5, 0.75, 1))
  
  # Separa os coeficientes por termos (Estimado, Std. Error, t value, p-value)
  stat.coef  <- summary(reg_linear)$coefficients
  coef    <- stat.coef[,1]    # 1st column: coefficients (same as above)
  se.coef <- stat.coef[,2]    # 2nd column: se for each coef
  t.coef  <- stat.coef[,3]    # 3rd column: t-value for each coef
  p.coef  <- stat.coef[,4]    # 4th column: p-value for each coefficient
  
  # divide os elementos do coeficiente da Estimativa
  CoefEst_Intercept <- stat.coef[1,1]
  CoefEst_irr_est <- stat.coef[2,1]
  CoefEst_temp <- stat.coef[3,1]
  CoefEst_numPM1 <- stat.coef[4,1]
  CoefEst_massaPM1 <- stat.coef[5,1]
  CoefEst_numPM2 <- stat.coef[6,1]
  CoefEst_massaPM2 <- stat.coef[7,1] 
  
  # divide os elementos do coeficiente da Erro Padrao (Std. Error)
  CoefStdError_Intercept <- stat.coef[1,2]
  CoefStdError_irr_est <- stat.coef[2,2]
  CoefStdError_temp <- stat.coef[3,2]
  CoefStdError_numPM1 <- stat.coef[4,2]
  CoefStdError_massaPM1 <- stat.coef[5,2]
  CoefStdError_numPM2 <- stat.coef[6,2]
  CoefStdError_massaPM2 <- stat.coef[7,2] 
  
  # divide os elementos do coeficiente dos p-Values
  CoefPValue_Intercept <- stat.coef[1,4]
  CoefPValue_irr_est <- stat.coef[2,4]
  CoefPValue_temp <- stat.coef[3,4]
  CoefPValue_numPM1 <- stat.coef[4,4]
  CoefPValue_massaPM1 <- stat.coef[5,4]
  CoefPValue_numPM2 <- stat.coef[6,4]
  CoefPValue_massaPM2 <- stat.coef[7,4]
  
  
  if (FALSE){
    TEASASJKJ
    ASKÇLAJNS
    EWEWEWE
    ÇmçnpçNAS
  }
  
  # elementos do rodapé do summary(reg_linear)
  summary(reg_linear)$r.squared           # Multiple R-squared
  summary(reg_linear)$adj.r.squared       # Adjusted R-squared:
  summary(reg_linear)$df[2]               # degrees of freedom
  summary(reg_linear)$sigma               # Residual standard error:

  #------------------------------------------------------------------------------------
  
  RL_atual <- data.frame(Dia = c(aux_dia[i]), 
                         RL_Res_Min = c(ResMin), 
                         RL_Res_1Q = c(Res_1Q),
                         RL_Res_Mdn = c(Res_Mdn),
                         RL_Res_3Q = c(Res_3Q),
                         RegLin_Max = c(Res_Max),   
                         
                         CoefEst_Intercept = c(CoefEst_Intercept),
                         CoefEst_irr_est = c(CoefEst_irr_est),
                         CoefEst_temp = c(CoefEst_temp),
                         CoefEst_numPM1 = c(CoefEst_numPM1),
                         CoefEst_massaPM1 = c(CoefEst_massaPM1), 
                         CoefEst_numPM2 = c(CoefEst_numPM2),
                         CoefEst_massaPM2 = c(CoefEst_massaPM2),
                         
                         CoefStdError_Intercept = c(CoefStdError_Intercept),
                         CoefStdError_irr_est = c(CoefStdError_irr_est),
                         CoefStdError_temp = c(CoefStdError_temp),
                         CoefStdError_numPM1 = c(CoefStdError_numPM1),
                         CoefStdError_massaPM1 = c(CoefStdError_massaPM1),
                         CoefStdError_numPM2 = c(CoefStdError_numPM2),
                         CoefStdError_massaPM2 = c(CoefStdError_massaPM2),
                         
                         CoefPValue_Intercept = c(CoefPValue_Intercept),
                         CoefPValue_irr_est = c(CoefPValue_irr_est),
                         CoefPValue_temp = c(CoefPValue_temp),
                         CoefPValue_numPM1 = c(CoefPValue_numPM1),
                         CoefPValue_massaPM1 = c(CoefPValue_massaPM1),
                         CoefPValue_numPM2 = c(CoefPValue_numPM2),
                         CoefPValue_massaPM2 = c(CoefPValue_massaPM2)
                         
                         )
  
  RL_final <- rbind(RL_final, RL_atual)
  # View(RL_final)
  
  #------------------------------------------------------------------------------------
  #sink("lm.txt")
  #reg_linear <- lm(modelo, data = dataReg)
  #print(summary(reg_linear))
  #sink()
  #------------------------------------------------------------------------------------
  #library(broom)
  #reg_linear <- lm(modelo, data = dataReg)
  #write.csv(tidy(reg_linear), "coefs.csv")     # tidy() function for the coefficients
  #write.csv(glance(reg_linear), "an.csv")      # glance() for the table.
  #------------------------------------------------------------------------------------
  
  #  z <- cor.test(df$temp, df$irr_inv)
  #  z <- cor(df$temp, df$irr_inv)
  
  
}