#    https://www.wxforum.net/index.php?PHPSESSID=9e6242ddde79abbe07549332715a0555&
#    http://colaweb.gmu.edu/dev/clim301/lectures/wind/wind-uv
#    http://mmc2.geofisica.unam.mx/cursos/geoest/Articulos/Geostatistics/Non-Linear%20Surface%20Interpolations.htm
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

#i = 1

j = 1
k = 10

# for(i in 1:length(nameDustDep)){ 
for(i in j:k){ 

  # assign(nameDustDep[i],read.csv(nameDustDep[i],skip=1, header=TRUE))
  x <- readr::read_csv(nameDustDep[i], col_types = cols(hora_minuto = col_character()))
  
  # Temperatura media em °C
  Temp_Media = round(mean(x$temp), digits = 5)
  
  # Direcao do Vento http://tornado.sfsu.edu/geosciences/classes/m430/Wind/WindDirection.html
  # https://www.wxforum.net/index.php?topic=8660.0
  C_zonal_U      = round( -x$vento_vel * sind(x$vento_dir) , 6)
  C_meridional_V = round( -x$vento_vel * cosd(x$vento_dir) , 6)
  
  
  # atan(1)*180/pi  #  atan(1) = 45° = 0.78539816 rad
  #DVr = round( atan2 (   (sum(C_zonal_U))  , (sum(C_meridional_V))  )  *180/pi , 6 )
  DVr1 = round( atan (sum(C_zonal_U) / sum(C_meridional_V))  , 6 ) 
  DVr2 = round( atan2 (sum(C_zonal_U) , sum(C_meridional_V)) , 6 ) 
  
  #  https://www.tandfonline.com/doi/pdf/10.1080/10473289.2003.10466276?needAccess=true
  # sin (1.57079633 rad) = 1
  # max(DVrAux)  = 1.537659
  IDV1 = round (1 + sin(DVr1-(-0.03313127))   , 6)
  IDV2 = round (1 + sin(DVr2-(-0.03313127))   , 6)
  
  aux_DirVento <- rbind(aux_DirVento, list(DVr1, IDV1, DVr2, IDV2), deparse.level = 1)
  
  #
  # ao utilizar a funcao atan eh encontrado o problema de descontinuidade 
  # nos intervalos de [0° - 90°, 180° - 270°] quando eh utilizada a funcao
  # atan2, o proprio software já corrige o problema de descontinuidade 
  # dividindo os termos por -pi
  #
  # a funcao atan encontra um problema com DVr < 0, mostrando os angulos 
  # complementares (quadrantes opostos) a funcao atan2 não apresenta
  #  esse problema, e os valores estão nos quadrantes corretos
  # 
  # https://en.wikipedia.org/wiki/Atan2
  # 
  
  
  # calcula a vel. do vento media atual e monta uma lista delas
  Vel_Med_Vento = round(mean(x$vento_vel), digits = 5)
  ListaVento <- c(ListaVento, Vel_Med_Vento)
  
  # calcula a media das velocidades medias e monta uma lista
  Media_Vel_Med_Vento = mean(ListaVento)
  ListaVentoMedia <- c(ListaVentoMedia, Media_Vel_Med_Vento)
  
  # calcula a media da massa dos particulados e monta uma lista 
  MassaParticulados = round(mean(x$massaPM1) + mean(x$massaPM2), digits = 8)
  ListaMassa <- c(ListaMassa, MassaParticulados)
  
  # calcula a media da massa media dos particulados e monta uma lista
  MassaMedia <- mean(ListaMassa)
  ListaMassaMedia <- c(ListaMassaMedia, MassaMedia)
  
  #######################################################
  
  ConcentracaoParticulados = round(mean(x$numPM1) + mean(x$numPM2), digits = 8)
  ListaConcentracao <- c(ListaConcentracao, ConcentracaoParticulados)

  ConcentracaoMediaParticulados <- mean(ListaConcentracao)
  ListaConcentracaoMedia <- c(ListaConcentracaoMedia, ConcentracaoMediaParticulados)
  
  
  
  # chamando trecho de codigo que possui o equacionamento
  source("D:/github/Tabelas_DynamoDB/RProject/scripts/e_dust_deposition_Part2.R")
  
  # criacao do dataset para calculo do impacto da sujidade
  dataset_DustDep <- rbind(dataset_DustDep, list(x$dia_mes_ano[1], Vd1, Pd, Nloss, 
                                                 Vd2, m, x_gauss, SR), deparse.level = 1)
  
  
}

# salva o dataset auxiliar de coordenada de dir/vel do vento
aux_DirVento <- aux_DirVento[-c(1),]
salvarArq_name <- paste(reglinear_caminho, "wind_test-MOD(", j, "-", k, ")_", aux_dia[j], "_", aux_dia[k], ".csv", sep = "")
write_csv(aux_DirVento, salvarArq_name)



# salva o dataset correspondente ao acumulo de massa
dataset_DustDep <- dataset_DustDep[-c(1),]
write_csv(dataset_DustDep,'D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\dust_deposition\\m_accumalation-MOD.csv')



# salva o dataset auxiliar de coordenada de dir/vel do vento
aux_DirVento <- aux_DirVento[-c(1),]
salvarArq_name <- paste("D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\dust_deposition\\m_accumalation-MOD", "wind_test-MOD(", j, "-", k, ")_", aux_dia[j], "_", aux_dia[k], ".csv", sep = "")
write_csv(aux_DirVento, salvarArq_name)



# salva o dataset correspondente ao acumulo de massa
dataset_DustDep <- dataset_DustDep[-c(1),]
salvarArq_name2 <- paste("reglinear_caminho", "m_accumalation-MOD(", j, "-", k, ")_", aux_dia[j], "_", aux_dia[k], ".csv", sep = "")
write_csv(dataset_DustDep, salvarArq_name2)








