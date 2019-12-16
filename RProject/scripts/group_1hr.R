




for(i in 1:length(filesMerge)){ 
  
  arquivos <- filesMerge[i]
  
  x <- readr::read_csv(arquivos, col_types = cols(hora_minuto = col_character()))
  
  g <- dplyr::group_by(x, dia_mes_ano, h = substr(hora_minuto, 1, 2), 
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
  
  salvarArq_name <- paste(pathGroup, "tabela_por_hora", dia, ".csv", sep = "")
  write_csv(y, salvarArq_name)
  
}