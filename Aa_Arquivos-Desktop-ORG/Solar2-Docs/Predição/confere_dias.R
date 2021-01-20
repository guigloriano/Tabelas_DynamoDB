

ajusta_dias <- read.csv("C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\TodosDias.csv")

ajusta_dias2 <- ajusta_dias[, c("dia_mes_ano", "dia", "TIME", "irr_inv", "temp", "m", "P_DC")]



linha_ajusta <- nrow(ajusta_dias2)


ajusta_dias2$dia <- ajusta_dias2$dia - 12

ajusta_dias2$dia[ajusta_dias2$dia < 1] <- NA
ajusta_dias2 <- na.exclude(ajusta_dias2)

caminho_salvar <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"

fileDest1 <- paste(caminho_salvar,  "/TodosDias2.csv", sep = "")
write.csv(ajusta_dias2, fileDest1) 
