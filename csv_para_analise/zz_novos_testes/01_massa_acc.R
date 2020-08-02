#install.packages("pracma")
#install.packages("data.table")               # Install data.table package
#install.packages("NORMT3")                   # para usar ERF 
library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(NORMT3)


csvAmbientais <- setwd("D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\ambientais\\")
csvAmbientais <- setwd("D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\ambientais\\")

caminho_ambientais <- setwd(csvAmbientais)
caminho_ambientais <- setwd(csvAmbientais)
nomesCsvAmbientais <- list.files(pattern = "*.csv")
arquivos_ambientais <- paste(caminho_ambientais,  "/", nomesCsvAmbientais, sep = "")



datasetMacc_aux <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
               c("dia", "Vd1", "Pd", "Nloss", 
                 "Vd2", "m", "x_gauss"))
#                 "Vd2", "m", "x_gauss", "SR" ))

#test <- setNames(data.frame(matrix(ncol = 4, nrow = 1)),
#                 c("DVr1", "IDV1", "DVr2" , "IDV2"))


ListaTemperaturaMedia <- NULL
ListaVento <- NULL
ListaVentoMedia <- NULL
ListaMassa <- NULL
ListaMassaMedia <- NULL
ListaConcentracao <- NULL
ListaConcentracaoMedia <- NULL


UAux <- NULL
RaAux <- NULL
RbAux <- NULL
VsAux <- NULL
VdAux <- NULL


dia_atual <- 1
dia_inicio <- 1

conta_dias <- NULL        # VERIFICAR UTILIDADE



for(dia_atual in dia_inicio:length(nomesCsvAmbientais)){
#for(dia_atual in dia_inicio:3){  
  
  dataset <- readr::read_csv(nomesCsvAmbientais[dia_atual], col_types = cols(hora_minuto = col_character()))
  dataset$X1 <- NULL
  
  
  qtd_linhas <- nrow(dataset)
  linha <- 1                        # VERIFICAR UTILIDADE

  
  cont_dias <- dia_atual
  
  if (qtd_linhas != 0){
    
    
    # Temperatura media em °C
    Temp_Media = round(mean(dataset$temp, na.rm=TRUE), digits = 5)
    ListaTemperaturaMedia <- c(ListaTemperaturaMedia, Temp_Media)
    
    # calcula a vel. do vento media atual e monta uma lista delas
    Vel_Med_Vento = round(mean(dataset$vento_vel, na.rm=TRUE), digits = 5)
    ListaVento <- c(ListaVento, Vel_Med_Vento)
    
    # calcula a media das velocidades medias e monta uma lista
    Media_Vel_Med_Vento = mean(ListaVento, na.rm=TRUE)
    ListaVentoMedia <- c(ListaVentoMedia, Media_Vel_Med_Vento)

    
    #ConcentracaoParticulados <- mean(dataset$numPM1, na.rm=TRUE) + mean(dataset$numPM2, na.rm=TRUE)
    ConcentracaoParticulados <- mean(dataset$numPM1, na.rm=TRUE) + mean(dataset$numPM2, na.rm=TRUE)
    ListaConcentracao <- c(ListaConcentracao, ConcentracaoParticulados)
    
    ConcentracaoMediaParticulados <- mean(ListaConcentracao, na.rm=TRUE)
    ListaConcentracaoMedia <- c(ListaConcentracaoMedia, ConcentracaoMediaParticulados)
    
    # calcula a media da massa dos particulados e monta uma lista 
    Massa_Particulados <- mean(dataset$massaPM1, na.rm=TRUE) + mean(dataset$massaPM2, na.rm=TRUE)
    ListaMassa <- c(ListaMassa, Massa_Particulados)
    
    # calcula a media da massa media dos particulados e monta uma lista
    Media_Massa_Particulados <- mean(ListaMassa, na.rm=TRUE)
    ListaMassaMedia <- c(ListaMassaMedia, Media_Massa_Particulados)
    
    # Ra = resistencia aerodinamica
    # Cds = coeficiente de arrasto da superfície [ 1.2 * 10^(-2) para oceanos, 1.2*10^(-2) para terra ]
    # U = velocidade media do vento (m/s)
    Cds = 1.0*10^(-2) # 0.012
#    U_velMed_ar = ListaVentoMedia[length(ListaVentoMedia)] / 3.6
    U_velMed_ar = mean(ListaVentoMedia) / 3.6
    UAux <- c(UAux, U_velMed_ar)
    
    Ra = 1/(Cds * U_velMed_ar)
    RaAux <- c(RaAux, Ra)
    
    # U_estrela = velocidade de atrito
    # constante de Von Karman [ VonKarman = 0.41 ]
    # h = altura de referencia [m ?]
    # z0 = 1, dimensao de rugosidade para o caso de terrenos urbanos
    VonKarman = 0.41
    h_ref = 2.5
    z0 = 1
    U_estrela = VonKarman * U_velMed_ar/ (log(h_ref/z0)) # U_estrela = ((0.41*U) / 0.9162907)

    # D = coeficiente de difusa browniano [ m²/s ]
    # k = constante de Boltzmann [ 1.38 * 10^(-23) J/k ] 
    # T_Ar = temperatura do ar [ em Kelvin, K = °C + 273,15 ]
    # u = viscosidade do ar [ 1.81 * 10^(-5) kg/ms ]
    # Dp = diametro da particula [ m , PM2.5 = 2.5*10^-(6)]
    k_boltzman = 1.38*10^(-23)
#    T_Ar = Temp_Media + 273.15
    T_Ar = mean(ListaTemperaturaMedia) + 273.15
    u_viscAr = 1.81*10^(-5)
    Dp_diamPart = 2.5*10^(-6)
    D_coefDif = k_boltzman*T_Ar/(3 * pi * u_viscAr * Dp_diamPart) # D = ((1.38e-23 * T_Ar)/ 4.264712e-10)
    
    # Sc = numero de Schmidt
    # v = viscosidade cinematica do ar [ 1.48 * 10^(-5) m²/s ]
    v_viscCinAr = 1.48*10^(-5)
    Sc_numSchmidt = v_viscCinAr/D_coefDif
    
    # Vs = velocidade de sedmentacao
    # Pp = densidade da particula aerosol [ 1000 kg/m³ ]
    # g = aceleracao gravitacional [ 9,81 m/s² ]
    # u = viscosidade do ar [ 1.81 * 10^(-5) kg/ms ]
    # Cc = fator de correcao de Cunninghann [ ~ 1 ]
    Pp_densPart = 1000
    g_acelGrav = 9.81
    Cc_fatCorr = 1
    Vs_velSed = ( (Dp_diamPart^2 * Pp_densPart * g_acelGrav)/(18 * u_viscAr) ) * Cc_fatCorr  # Vs= 0.0001881906
    
    # St = numero de Strokes
    St_numStrokes = ( U_estrela^2 * Vs_velSed)/(g_acelGrav * v_viscCinAr)
    
    # Rb = resistencia da camada quasi-laminar
    Rb = 1/(3 * U_estrela * (Sc_numSchmidt^(-0.5) + (St_numStrokes^(2)/(1+St_numStrokes^(2) ) ) ) )
    RbAux <- c(RbAux, Rb)
    
    # theta = inclinacao dos modulos [ em ° ]
    theta = 15
    
    ### Modelo 01 - On temporal modelling (...) in seven cities
    # Vd = velocidade de deposicao 
    Vd1 = 1/(Ra+Rb) + Vs_velSed*cos(theta*pi/180)
    Pd = round( Vd1 * ListaConcentracaoMedia[length(ListaConcentracaoMedia)] * 10^(-6) * (cont_dias+1), 8) #MediaConcentracao
    Nloss = 0.015 * Pd
    
    ### Modelo 02 - Simple Model for Predicting (...) of PV Panels
    # t unidade de tempo em segundos
    t_sec = 86400 * cont_dias
    Vd2 =  1/(Ra+Rb) + Vs_velSed 
    VdAux <- c(VdAux, Vd2)
    
    m = Vd2 * mean(ListaMassaMedia) * 10^(-6) * cos(theta*pi/180) * t_sec # -,,,
    x_gauss = 0.17*m^(0.8473)
#    SR = 1 - 34.37*erf(x_gauss)
    

    datasetMacc_aux <- rbind(datasetMacc_aux, list(dataset$dia_mes_ano[1], Vd1, Pd, Nloss, 
                         Vd2, m, x_gauss), deparse.level = 1)
#                         Vd2, m, x_gauss, SR), deparse.level = 1)
    
#    i <- i + 1
  }
  
}

datasetMacc_aux <- datasetMacc_aux[-c(1),]
#write_csv(datasetMacc_aux,'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\m_accumalation.csv')
