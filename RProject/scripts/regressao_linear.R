for(i in 1:5){   
  
  i = 1
  
  modelo <- x$P_AC ~ x$irr_est + x$temp + x$numPM1 + x$massaPM1 + x$numPM2 + x$massaPM2
  
  
  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  x <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))
  reg_linear <- lm(modelo, data = x)
  summary(reg_linear)
  y <- cor(x)
  
  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  dataset <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))
  multi.fit <- lm(modelo, dataset)
  summary(multi.fit)
  
  
  df.coef <- as.data.frame( coef(summary(reg_linear)) )
  teste2.data.frame <- summary(reg_linear)$coefficients
  
  
  multi.fit = lm(P_AC ~ irr_est + temp, dataset)
  summary(multi.fit)
  
  save (reg_linear, "Teste.RData")
}