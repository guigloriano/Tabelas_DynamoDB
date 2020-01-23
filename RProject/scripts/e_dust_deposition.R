#    https://www.wxforum.net/index.php?PHPSESSID=9e6242ddde79abbe07549332715a0555&
#    http://colaweb.gmu.edu/dev/clim301/lectures/wind/wind-uv
#    http://mmc2.geofisica.unam.mx/cursos/geoest/Articulos/Geostatistics/Non-Linear%20Surface%20Interpolations.htm
#    https://www.researchgate.net/post/How_to_interpolate_wind_direction_in_GIS_using_transformation
#

df <- setNames(data.frame(matrix(ncol = 8, nrow = 1)), 
               c("dia", "Vd1", "Pd", "Nloss", 
                 "Vd2", "m", "x_gauss", "SR" ))

test <- setNames(data.frame(matrix(ncol = 4, nrow = 1)),
                 c("DVr1", "IDV1", "DVr2" , "IDV2"))


auxVento <- 0
auxVentoMedia <- 0

auxMassa <- 0
auxMassaMedia <- 0

auxConcentracao <- 0
auxConcentracaMedia <- 0

UAux <- NULL
RaAux <- NULL
RbAux <- NULL
VsAux <- NULL
VdAux <- NULL



for(i in 1:length(nameDustDep)){ 
  
  # i = 1
  #assign(nameDustDep[i],read.csv(nameDustDep[i],skip=1, header=TRUE))
  
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
  
  test <- rbind(test, 
                list(DVr1, IDV1, DVr2, IDV2), 
                deparse.level = 1)
  
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
  
  
  
  # Vel_Med_Vento eh dado em km/h
  Vel_Med_Vento = round(mean(x$vento_vel), digits = 5)
  auxVento <- c(auxVento, Vel_Med_Vento)
  if (auxVento[1] == 0){
    ListaVento <- auxVento[-1]
  }
  
  Media_Vel_Med_Vento = mean(ListaVento)
  auxVentoMedia <- c(auxVentoMedia, Media_Vel_Med_Vento)
  if (auxVentoMedia[1] == 0){
    ListaVentoMedia <- auxVentoMedia[-1]
  }
  
  
  # Soma da Massa dos Particulados ug/m³
  MassaParticulados = round(mean(x$massaPM1) + mean(x$massaPM2), digits = 8)
  auxMassa <- c(auxMassa, MassaParticulados)
  if (auxMassa[1] == 0){
    ListaMassa <- auxMassa[-1]
  }
  
  MassaMedia <- mean(ListaMassa)
  auxMassaMedia <- c(auxMassaMedia, MassaMedia)
  if (auxMassaMedia[1] == 0){
    ListaMassaMedia <- auxMassaMedia[-1]
  }
  
  
  #######################################################
  
  ConcentracaoParticulados = round(mean(x$numPM1) + mean(x$numPM2), digits = 8)
  auxConcentracao <- c(auxConcentracao, ConcentracaoParticulados)
  if (auxConcentracao[1] == 0){
    ListaConcentracao <- auxConcentracao[-1]
  }
  
  ConcentracaoMediaParticulados <- mean(ListaConcentracao)
  auxConcentracaMedia <- c(auxConcentracaMedia, ConcentracaoMediaParticulados)
  if (auxConcentracaMedia[1] == 0){
    ListaConcentracaoMedia <- auxConcentracaMedia[-1]
  }
  
  # Ra = resistencia aerodinamica
  # Cds = coeficiente de arrasto da superfície [ 1.2 * 10^(-2) ]
  # U = velocidade media do vento (m/s)
  Cds = 1.2*10^(-2)
  U = ListaVentoMedia[i] / 3.6
  
  UAux <- c(UAux, U)
  Ra = 1/(Cds * U)
  
  RaAux <- c(RaAux, Ra)
  
  # U_estrela = velocidade de atrito
  # constante de Von Karman [ VonKarman = 0.41 ]
  # h = altura de referencia [m ?]
  # z0 = 1, dimensao de rugosidade para o caso de terrenos urbanos
  VonKarman = 0.41
  h = 2.5
  z0 = 1
  U_estrela = VonKarman * U/ (log(h/z0))
  
  # D = coeficiente de difusa browniano [ m²/s ]
  # k = constante de Boltzmann [ 1.38 * 10^(-23) J/k ] 
  # T_Ar = temperatura do ar [ em Kelvin, K = °C + 273,15 ]
  # u = viscosidade do ar [ 1.81 * 10^(-5) kg/ms ]
  # Dp = diametro da particula [ m , PM2.5 = 2.5*10^-(6)]
  k = 1.38*10^(-23)
  T_Ar = Temp_Media + 273.15
  u = 1.81*10^(-5)
  Dp = 2.5*10^(-6)
  D = k*T_Ar/(3 * pi * u * Dp )
  
  # Sc = numero de Schmidt
  # v = viscosidade cinematica do ar [ 1.48 * 10^(-5) m²/s ]
  v = 1.48*10^(-5)
  Sc = v/D
  
  # Vs = velocidade de sedmentacao
  # Pp = densidade da particula aerosol [ 1000 kg/m³ ]
  # g = aceleracao gravitacional [ 9,81 m/s² ]
  # u = viscosidade do ar [ 1.81 * 10^(-5) kg/ms ]
  # Cc = fator de correcao de Cunninghann [ ~ 1 ]
  Pp = 1000
  g = 9.81
  Cc = 1
  
  Vs = ( (Dp^2 * Pp * g)/(18 * u) ) * Cc
  
  VsAux <- c(VsAux, Vs)
  
  # St = numero de Strokes
  St = ( U_estrela^2 * Vs)/(g * v)
  
  # Rb = resistencia da camada quasi-laminar
  Rb = 1/(3 * U_estrela * (Sc^(-0.5) + (St/(1+St))^(2) ) )
  
  RbAux <- c(RbAux, Rb)
  
  # theta = inclinacao dos modulos [ em ° ]
  theta = 15
  
  ### Modelo 01 - On temporal modelling (...) in seven cities
  # Vd = velocidade de deposicao 
  Vd1 = 1/(Ra+Rb) + Vs*cosd(theta)
  
  Pd = round( Vd1 * ListaConcentracaoMedia[i] * 10^(-6) * i , 6) #MediaConcentracao
  Nloss = 0.015 * Pd
  
  ### Modelo 02 - Simple Model for Predicting (...) of PV Panels
  # t unidade de tempo em segundos
  t_sec = 86400*i
  Vd2 =  1/(Ra+Rb) + Vs 
  
  VdAux <- c(VdAux, Vd2)
  
  m = Vd2 * ListaMassaMedia[i] * 10^(-6) * cosd(theta) * t_sec # MediaMassa
  x_gauss = 0.17*m^(0.8473)
  SR = 1 - 34.37*erf(x_gauss)
  
  
  m
  x_gauss
  SR
  
  Pd
  Nloss
  
  
  
  df <- rbind(df, list(x$dia_mes_ano[1], Vd1, Pd, Nloss, 
                       Vd2, m, x_gauss, SR), deparse.level = 1)
  
  
}

test <- test[-c(1),]
write_csv(test,'D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\dust_deposition\\wind_test-ORI.csv')

df <- df[-c(1),]
write_csv(df,'D:\\github\\Tabelas_DynamoDB\\csv\\teste_lm\\dust_deposition\\m_accumalation-ORI.csv')
