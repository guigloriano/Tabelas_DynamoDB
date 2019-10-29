library(dplyr)
library(magrittr)


caminho_CSV <- "~/Projeto/Tabelas_DynamoDB/ambientais-cg-20191025.csv"
dadosEst <- read.csv(caminho_CSV, header=TRUE, sep = ",")
View(dadosEst)

PM1massa <- dadosEst$pm1_massa
PM1massaMedia <- mean(PM1massa)
View(PM1massa)





View(t)

t <- strptime(dadosEst$horario, "%H:%M")
t2 <- as.numeric(strftime(t, "H%M%S"))

x <- readr::read_csv("Downloads/Telegram Desktop/ambientais-cg-20191025.csv", col_types = cols(horario = col_character()))
g <- dplyr::group_by(x, data, h = substr(horario, 1, 2), m = floor(as.numeric(substr(horario, 4, 5))/15))
gg <- dplyr::summarise(g, hora = dplyr::first(horario), irradiacao = mean(irradiacao), pm1_massa = mean(pm1_massa), pm2_massa = mean(pm2_massa), pm4_massa = mean(pm4_massa), pm10_massa = mean(pm10_massa), pm1_concentracao = mean(pm1_concentracao), pm2_concentracao = mean(pm2_concentracao), pm4_concentracao = mean(pm4_concentracao), pm10_concentracao = mean(pm10_concentracao), concentracao_media = mean(concentracao_media), temperatura = mean(temperatura),vento_vel=mean(vento_vel), n = dplyr::n())
y <- gg
y$h <- NULL
y$m <- NULL
write_csv(y,'Downloads/Telegram Desktop/mestrado-glo.csv')


g = group_by(dataset, data, hora = substr(hora, 1, 2))
dplyr::summarise(g, hora = dplyr::first(hora), temp_ar_avg = mean(temp_ar_avg), 
                 umid_ar_avg = mean(umid_ar_avg), vel_vento = mean(vel_vento), 
                 dir_vento = mean(dir_vento), SD1_dir = mean(SD1_dir), 
                 vel_vento_max = max(vel_vento_max), vel_vento_min = min(vel_vento_min), 
                 press_atm_avg = mean(press_atm_avg), prec_chuva_tot = sum(prec_chuva_tot),
                 avg_radsol_H = mean(avg_radsol_H), radsol_H_max = max(radsol_H_max), 
                 radsol_H_min = min(radsol_H_min), avg_radsol_I = mean(avg_radsol_I),
                 radsol_I_max = max(radsol_I_max), radsol_I_min = min(radsol_I_min),
                 avg_radSNP1_glob = mean(avg_radSNP1_glob), radSPN1_glob_max = max(radSPN1_glob_max), 
                 radSPN1_glob_min = min(radSPN1_glob_min), avg_radSNP1_difusa = mean(avg_radSNP1_difusa),
                 radSPN1_difusa_max = max(radSPN1_difusa_max), radSPN1_difusa_min = min(radSPN1_difusa_min), 
                 irradiancia_avg = mean(irradiancia_avg), irradiancia_max = max(irradiancia_max), 
                 irradiancia_min = min(irradiancia_min), irradiancia_2_avg = mean(irradiancia_2_avg),
                 irradiancia_2_max = max(irradiancia_2_max), irradiancia_2_min = min(irradiancia_2_min), 
                 n = n())
                 
                 
                 