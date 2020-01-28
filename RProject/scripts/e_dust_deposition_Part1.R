#    http://colaweb.gmu.edu/dev/clim301/lectures/wind/wind-uv
#    https://www.researchgate.net/post/How_to_interpolate_wind_direction_in_GIS_using_transformation
#
dataset_DustDep <- setNames(data.frame(matrix(ncol = 8, nrow = 1)), 
                            c("dia", "Vd1", "Pd", "Nloss", 
                              "Vd2", "m", "x_gauss", "SR" ))

aux_DirVento <- setNames(data.frame(matrix(ncol = 4, nrow = 1)),
                 c("DVr1", "IDV1", "DVr2" , "IDV2"))

ListaVento <- NULL
ListaVentoMedia <- NULL
ListaMassa <- NULL
ListaMassaMedia <- NULL
ListaConcentracao <- NULL
ListaConcentracaoMedia <- NULL

aux_dia <- NULL

IN = 1
OUT = length(nameDustDep)

for(i in IN:OUT){ 

  #i = 1
  # assign(nameDustDep[i],read.csv(nameDustDep[i],skip=1, header=TRUE))
  dustDep_temp <- readr::read_csv(nameDustDep[i], col_types = cols(hora_minuto = col_character()))
  aux_dia[i] <- dustDep_temp$dia_mes_ano[1]
  
  # Temperatura media em °C
  Temp_Media = round(mean(dustDep_temp$temp), digits = 5)
  
  # etapa que calcula o indice de direção do vento 
  source("D:/github/Tabelas_DynamoDB/RProject/scripts/e_dust_deposition_Part1b.R")
  
  # calcula a vel. do vento media atual e monta uma lista delas
  Vel_Med_Vento = round(mean(dustDep_temp$vento_vel), digits = 5)
  ListaVento <- c(ListaVento, Vel_Med_Vento)
  
  # calcula a media das velocidades medias e monta uma lista
  Media_Vel_Med_Vento = mean(ListaVento)
  ListaVentoMedia <- c(ListaVentoMedia, Media_Vel_Med_Vento)
  
  # calcula a media da massa dos particulados e monta uma lista 
  Massa_Particulados = round(mean(dustDep_temp$massaPM1) + mean(dustDep_temp$massaPM2), digits = 8)
  ListaMassa <- c(ListaMassa, Massa_Particulados)
  
  # calcula a media da massa media dos particulados e monta uma lista
  Media_Massa_Particulados <- mean(ListaMassa)
  ListaMassaMedia <- c(ListaMassaMedia, Media_Massa_Particulados)
  
  #######################################################
  
  ConcentracaoParticulados = round(mean(dustDep_temp$numPM1) + mean(dustDep_temp$numPM2), digits = 8)
  ListaConcentracao <- c(ListaConcentracao, ConcentracaoParticulados)

  ConcentracaoMediaParticulados <- mean(ListaConcentracao)
  ListaConcentracaoMedia <- c(ListaConcentracaoMedia, ConcentracaoMediaParticulados)
  
  # chamando trecho de codigo que possui o equacionamento
  source("D:/github/Tabelas_DynamoDB/RProject/scripts/e_dust_deposition_Part2.R")
  
  # criacao do dataset para calculo do impacto da sujidade
  dataset_DustDep <- rbind(dataset_DustDep, list(dustDep_temp$dia_mes_ano[1], Vd1, Pd, Nloss, 
                                                 Vd2, m, x_gauss, SR), deparse.level = 1)
  
}

# salva o dataset auxiliar de coordenada de dir/vel do vento
aux_DirVento <- aux_DirVento[-c(1),]
salvarArq_name <- paste("D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\dust_deposition\\wind_test-MOD(", IN, "-", OUT, ")_", aux_dia[IN], "_", aux_dia[OUT], ".csv", sep = "")
write_csv(aux_DirVento, salvarArq_name)

# salva o dataset correspondente ao acumulo de massa
dataset_DustDep <- dataset_DustDep[-c(1),]
salvarArq_name2 <- paste("D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\dust_deposition\\m_accumalation-MOD(", IN, "-", OUT, ")_", aux_dia[IN], "_", aux_dia[OUT], ".csv", sep = "")
write_csv(dataset_DustDep, salvarArq_name2)

