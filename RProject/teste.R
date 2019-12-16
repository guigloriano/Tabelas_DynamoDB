library(dplyr)
library(readr)
library(stringr)
library(pracma)

pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_min/hora_corrigida/")
pathInv <- setwd("D:/github/Tabelas_DynamoDB/inversor_diario_min/hora_corrigida/")
namesInv <- list.files(pattern = "*.csv")
filesInv <- paste(pathInv,  "/", namesInv, sep = "")

pathSta <- setwd("D:/github/Tabelas_DynamoDB/ambientais_diario_min/")
pathSta <- setwd("D:/github/Tabelas_DynamoDB/ambientais_diario_min/")
namesSta <- list.files(pattern = "*.csv")
filesSta <- paste(pathSta, "/", namesSta, sep = "")

for(i in 1:length(filesInv)){ 
#  i = 1
  dfestacao <- filesSta[i]
  dfinversor <- filesInv[i]
  
  x_est <- readr::read_csv(dfestacao, col_types = cols(hora_minuto = col_character()))
  y_inv <- readr::read_csv(dfinversor, col_types = cols(hora_minuto = col_character()))
  
  x_est$hora_minuto <- str_pad(x_est$hora_minuto, width=6, side="left", pad="0")
  x_est$hora_minuto <- as.character(x_est$hora_minuto)
  
  y_inv$hora_minuto <- str_pad(y_inv$hora_minuto, width=6, side="left", pad="0")
  y_inv$hora_minuto <- as.character(y_inv$hora_minuto)
  
  z_merge <- merge.data.frame(x = x_est, y = y_inv, all = TRUE)
  
  # calculo da potencia dc
  z_merge$P_DC = z_merge$I_DC * z_merge$V_DC
  
## decomposicao da direcao do vento em coordenadas
#  C_zonal_U      = round( -z_merge$vento_vel * sind(z_merge$vento_dir) , 6)
#  C_meridional_V = round( -z_merge$vento_vel * cosd(z_merge$vento_dir) , 6)
#  z_merge$C_zonal_U <- C_zonal_U
#  z_merge$C_meridional_V <- C_meridional_V
#  DVr1 = round( atan (sum(C_zonal_U) / sum(C_meridional_V))  , 6) 
#  IDV1 = round (1 + sin(DVr1-(-0.03313127))   , 6)

  dia <- z_merge$dia_mes_ano[1]
  
  pathDest <- "D:/github/teste_lm/merge/"
  fileDest <- paste(pathDest,  "teste_lm_", dia, ".csv", sep = "")
  write_csv(z_merge, fileDest)
  
  g <- dplyr::group_by(z_merge, dia_mes_ano, h = substr(hora_minuto, 1, 2), 
                       m = floor(as.numeric(substr(hora_minuto, 3, 4))/60))
  
  gg <- dplyr::summarise(g, hora_minuto = dplyr::first(hora_minuto), 
                         
                         irr_est = round(mean(irr, na.rm=TRUE), digits = 2), 
                         irr_inv = round(mean(IRR, na.rm=TRUE), digits = 2),
                         
                         I_AC = round(mean(I_AC, na.rm=TRUE), digits = 2),
                         V_AC = round(mean(V_AC, na.rm=TRUE), digits = 2),
                         P_AC = round(mean(P_AC, na.rm=TRUE), digits = 2),
                         
                         I_DC = round(mean(I_DC, na.rm=TRUE), digits = 2),
                         V_DC = round(mean(V_DC, na.rm=TRUE), digits = 2),
                         P_DC = round(mean(P_DC, na.rm=TRUE), digits = 2),
                         
                         massaPM1 = round(mean(massaPM1, na.rm=TRUE), digits = 2), 
                         numPM1 = round(mean(numPM1, na.rm=TRUE), digits = 2), 
                         massaPM2 = round(mean(massaPM2, na.rm=TRUE), digits = 2), 
                         numPM2 = round(mean(numPM2, na.rm=TRUE), digits = 2), 
                         massaPM4 = round(mean(massaPM4, na.rm=TRUE), digits = 2), 
                         numPM4 = round(mean(numPM4, na.rm=TRUE), digits = 2), 
                         massaPM10 = round(mean(massaPM10, na.rm=TRUE), digits = 2), 
                         numPM10 = round(mean(numPM10, na.rm=TRUE), digits = 2), 
                         tamanho_medio = round(mean(tamanho_medio, na.rm=TRUE), digits = 2),
                         
                         temp = round(mean(temp, na.rm=TRUE), digits = 2),
                         vento_vel=round(mean(vento_vel, na.rm=TRUE), digits = 2),
                         vento_dir=round(mean(vento_dir, na.rm=TRUE), digits = 2), 
                         rainfall = max(rainfall, na.rm=TRUE),

                         n = dplyr::n())
  
  y <- gg
  y$h <- NULL
  y$m <- NULL
  y$n <- NULL
  
  pathDest2 <- "D:/github/teste_lm/group/"  
  salvarArq_name <- paste(pathDest2, "teste1_", dia, ".csv", sep = "")
  write_csv(y, salvarArq_name)
  
}

pathRegLin <- setwd("D:/github/teste_lm/group/" )
pathRegLin <- setwd("D:/github/teste_lm/group/" )
nameRegLin <- list.files(pattern = "*.csv")
filesRegLin <- paste(pathRegLin,  "/", nameRegLin, sep = "")


csvPath <- setwd("D:/github/teste_lm/group/")
names <- list.files(pattern = "*.csv")

#for(i in 1:length(filesRegLin)){ 
for(i in 1:5){   
  
  i = 4
  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  x <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))
  

  dfRegLin <- x
  reg_linear <- lm(formula = x$P_AC ~ x$irr_est + x$temp + x$numPM1 +x$massaPM1 + x$numPM2 + x$massaPM2, data = x)
  
  save (reg_linear, "Teste.RData");
  
  summary(reg_linear)
  df.coef <- as.data.frame( coef(summary(reg_linear)) )
  
  teste2.data.frame <- summary(reg_linear)$coefficients
}

