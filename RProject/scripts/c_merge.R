# este codigo combina as tabelas do Inversor e Estacao 
# de minuto em minuto em um único arquivo

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
  
  fileDest <- paste(pathMerge,  "/merge_", dia, ".csv", sep = "")
  write_csv(z_merge, fileDest)
  
}