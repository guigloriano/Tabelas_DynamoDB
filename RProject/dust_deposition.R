
#install.packages("pracma")
library(pracma)
library(readr)

csvPath <- setwd("D:\\github\\Tabelas_DynamoDB\\ambientais_diario_min\\")
names <- list.files(pattern = "*.csv")

for(i in 1:length(names)){ 
  i = 1
  assign(names[i],read.csv(names[i],skip=1, header=TRUE))
  x <- readr::read_csv(names[i], col_types = cols(hora_minuto = col_character()))

  # Vel_Med_Vento eh dado em km/h
  Vel_Med_Vento = round(mean(x$vento_vel), digits = 5)
  # Temperatura media em °C
  Temp_Media = round(mean(x$temp), digits = 5)
  # Soma da Massa dos Particulados ug/m³
  ConcentracaoMassa = round(mean(x$massaPM1) + mean(x$massaPM2), digits = 5)
  
  # Ra = resistencia aerodinamica
  # Cds = coeficiente de arrasto da superfície [ 1.2 * 10^(-2) ]
  # U = velocidade media do vento (m/s)
  Cds = 1.2*10^(-2)
  U = Vel_Med_Vento / 3.6
  Ra = 1/(Cds * U)
  
  # U_estrela = velocidade de atrito
  # constante de Von Karman [ VonKarman = 0.41 ]
  # h = altura de referencia [m ?]
  # z0 = 1, dimensao de rugosidade para o caso de terrenos urbanos
  VonKarman = 0.41
  h = 2.5
  z0 = 1
  U_estrela = VonKarman * U/ (log(h/z0))
  
  # D = coeficiente de difusa browniano [ m²/s ]
  # k = constante de Boltzmann [ 1.38 * 10^(-23) J/k]
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
  Vs = ((1 * (Dp^2) * Pp * g)/(18 * u)) * Cc
  
  # St = numero de Strokes
  St = ( U_estrela^2 * Vs)/(g * v)
  
  # Rb = resistencia da camada quasi-laminar
  Rb = 1/(3 * U_estrela * (Sc^(-0.5) + (St/(1+St))^(2) ) )
  
  # theta = inclinacao dos modulos [ em ° ]
  theta = (15*pi)/180
  
  ### Modelo 01 - On temporal modelling (...) in seven cities
  # Vd = velocidade de deposicao 
  Vd1 = 1/(Ra+Rb) + Vs*cos(theta)
  Pd = Vd1 * ConcentracaoMassa * 10^(-6)
  Nloss = 0.015 * Pd

  ### Modelo 02 - Simple Model for Predicting (...) of PV Panels
  # t unidade de tempo em segundos
  t_sec = 86400
  Vd2 = 1/(Ra+Rb) + Vs
  
  m = Vd2 * ConcentracaoMassa * 10^(-6)  * cos(theta) * t_sec
  x_gauss = 0.17*m^(0.8473)
  SR = 1 - 34.37*erf(x_gauss)

}
