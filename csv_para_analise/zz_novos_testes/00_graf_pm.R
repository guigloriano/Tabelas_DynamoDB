library(dplyr)
library(readr)
library(stringr)
library(pracma)
library(compareDF)
library(lubridate)
library(chron)
library(scales) 
library(lubridate)

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(RColorBrewer)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(quantmod)

require(tidyverse)
library(xts)
library(zoo)
library(reshape2)    

#####################################################################
#########################  MENU DE OPÇÕES  ##########################
#                                                                   #
#    OP_DIARIO = 1: Massa Diária de Particulados                    #
#    OP_DIARIO = 2: Concentração Diária de Particulados             #   
#    OP_DIARIO = 3: Massa e Concentração Diaria de Particulados     #
#                                                                   #
#    OP_M_ACC = 1 : Calcula a massa acumulada no decorrer dos dias  #
#                                                                   #
#    OP_GERAL = 1 : Graf. de Massa p/ todos os dias - 4 PMs         #
#    OP_GERAL = 2 : Graf. de Concentração p/ todos os dias - 4 PMs  #
#    OP_GERAL = 3 : Graf. de Massa e Conc. p/ todos os dias - 4 PMs #
#    OP_GERAL = 4 : A FAZER                                         #
#                                                                   #
#    OP_CHUVA = 1 : Grafico para Precipitação Pluviométrica         #
#    OP_CHUVA = 2 : Grafico contendo Concent. e Precip. Pluviom.    #
#    OP_CHUVA = 3 : Grafico contendo Macc. e Precip. Pluviom.       #
#    OP_CHUVA_T=1 : Impressao da chuva com grafico de segmentos     #
#    OP_CHUVA_T=2 : Impressao da chuva com grafico de barras        #
#                                                                   #
#    OP_M_CHUVA=1 : A FAZER                                         #
#                                                                   #
#####################################################################


######## VARIAVEIS DE CONTROLE #############

OP_DIARIO   <- 0
OP_M_ACC    <- 0
OP_GERAL    <- 3
OP_CHUVA    <- 0
OP_CHUVA_T  <- 2    # pode deixar setado
OP_M_CHUVA  <- 1

###########################################


########### VARIAVEIS GLOBAIS #############

caminhoDados <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\ambientais\\"

caminhoAmbientais <- setwd(caminhoDados)
caminhoAmbientais <- setwd(caminhoDados)
listaDadosAmbientais <- list.files(pattern = "*.csv")
csvDadosAmbientais <- paste(caminhoAmbientais,  "/", listaDadosAmbientais, sep = "")

dias <- 1
datasetAux <- NULL

datasetMacc_aux <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                        c("dia", "Vd1", "Pd", "Nloss", "Vd2", "m", "x_gauss"))
#                 "Vd2", "m", "x_gauss", "SR" ))

datasetMacc2_aux <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                            c("dia", "Vd1", "Pd", "Nloss", "Vd2", "m", "x_gauss"))

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

#dias <- 2

for(dias in 1:length(listaDadosAmbientais)){ 
# for(dias in 1:1){   
  
      df <- listaDadosAmbientais[dias]
      dataset <- readr::read_csv(df, col_types = cols(hora_minuto = col_character()))
      qtd_amostras <- nrow(dataset)
  
      if (qtd_amostras != 0){
        diarioGroup <- dplyr::group_by(dataset, dia_mes_ano, h = substr(hora_minuto, 1, 2), 
                                       m = floor(as.numeric(substr(hora_minuto, 3, 4))/30))
        
        diarioSummarise <- dplyr::summarise(diarioGroup, hora_minuto = dplyr::first(hora_minuto), 
                                            irradiacao = mean(irr), 
                                            massaPM1 = sum(massaPM1), 
                                            massaPM2 = sum(massaPM2), 
                                            massaPM4 = sum(massaPM4), 
                                            massaPM10 = sum(massaPM10), 
                                            numPM1 = sum(numPM1), 
                                            numPM2 = sum(numPM2),
                                            numPM4 = sum(numPM4), 
                                            numPM10 = sum(numPM10), 
                                            tamanho_medio = mean(tamanho_medio),
                                            temp = mean(temp),
                                            vento_dir =mean(vento_dir),
                                            vento_vel =mean(vento_vel), 
                                            n = dplyr::n())
        
        datasetDiario <- diarioSummarise
        datasetDiario$h <- NULL
        datasetDiario$m <- NULL
        datasetAux <- rbind(datasetAux, dataset)
        
        cont_dias <- dias
        
        # GRAFICOS COM A MASSA DIARIA DE PARTICULADOS
        if (OP_DIARIO == 1){
      
        #######################################################################
        #########     GRAFICOS COM A MASSA DIARIA DE PARTICULADOS #############
          
          dia_PM1 <- ggplot() +
            geom_point(data = datasetDiario, aes(hora_minuto, massaPM1, group = 1)) +
            geom_line(data = datasetDiario, aes(hora_minuto, massaPM1, group = 1)) +
            labs(x = "Dia", y = "Massa da Partícula [ug/cm³]") +
            ggtitle("Massa Diária") +
            theme(axis.text.x = element_text(angle = 45))
      
          dia_PM2 <- ggplot() +
            geom_point(data = datasetDiario, aes(hora_minuto, massaPM2, group = 2)) +
            geom_line(data = datasetDiario, aes(hora_minuto, massaPM2, group = 2)) +
            labs(x = "Dia", y = "Massa da Partícula [ug/cm³]") +
            ggtitle("Massa Diária") +
            theme(axis.text.x = element_text(angle = 45))
      
          dia_PM4 <- ggplot() + 
            geom_point(data = datasetDiario, aes(hora_minuto, massaPM4, group = 3)) +
            geom_line(data = datasetDiario, aes(hora_minuto, massaPM4, group = 3)) +
            labs(x = "Dia", y = "Massa da Partícula [ug/cm³]") +
            ggtitle("Massa Diária") +
            theme(axis.text.x = element_text(angle = 45)) 
          
          dia_PM10 <- ggplot() + 
            geom_point(data = datasetDiario, aes(hora_minuto, massaPM10, group = 4)) +
            geom_line(data = datasetDiario, aes(hora_minuto, massaPM10, group = 4)) +
            labs(x = "Dia", y = "Massa da Partícula [ug/cm³]") +
            ggtitle("Massa Diária") + 
            theme(axis.text.x = element_text(angle = 45)) 
          
          #if (imprimir == 1){
          if (1 == 1){
              grid_dia <- grid.arrange(dia_PM1, dia_PM2, dia_PM4, dia_PM10, 
                                       ncol=2, nrow=2, top="Massa Diária dos Particulados")
              pathDest1 <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\gf_massa\\"
              fileDest1 <- paste(pathDest1,  "Dia_", dataset$dia_mes_ano[1], ".png", sep = "")
              png(filename = fileDest1, width = 1280, height = 720, units = 'px')
              plot(grid_dia)
              dev.off() 
          }
        }
        
        # GRAFICOS COM A CONCENTRACAO DIARIA DE PARTICULADOS
        if (OP_DIARIO == 2){
          
        ##############################################################################
        #########     GRAFICOS COM A CONCENTRACAO DIARIA DE PARTICULADOS #############
          
          dia_nPM1 <- ggplot() +
            geom_point(data = datasetDiario, aes(hora_minuto, numPM1, group = 1)) +
            geom_line(data = datasetDiario, aes(hora_minuto, numPM1, group = 1)) +
            labs(x = "Horário", y = "Concentração da Partícula [#/cm³]") +
            ggtitle("Concentração Diária") +
            theme(axis.text.x = element_text(angle = 60))
          
          dia_nPM2 <- ggplot() +
            geom_point(data = datasetDiario, aes(hora_minuto, numPM2, group = 2)) +
            geom_line(data = datasetDiario, aes(hora_minuto, numPM2, group = 2)) +
            labs(x = "Horário", y = "Concentração da Partícula [#/cm³]") +
            ggtitle("Concentração Diária") +
            theme(axis.text.x = element_text(angle = 60))
          
          dia_nPM4 <- ggplot() + 
            geom_point(data = datasetDiario, aes(hora_minuto, numPM4, group = 3)) +
            geom_line(data = datasetDiario, aes(hora_minuto, numPM4, group = 3)) +
            labs(x = "Horário", y = "Concentração da Partícula [#/cm³]") +
            ggtitle("Concentração Diária") +
            theme(axis.text.x = element_text(angle = 60)) 
          
          dia_nPM10 <- ggplot() + 
            geom_point(data = datasetDiario, aes(hora_minuto, numPM10, group = 4)) +
            geom_line(data = datasetDiario, aes(hora_minuto, numPM10, group = 4)) +
            labs(x = "Horário", y = "Concentração da Partícula [#/cm³]") +
            ggtitle("Concentração Diária") +  
            theme(axis.text.x = element_text(angle = 60)) 
          
          #if (imprimir == 1){
          if (1 == 1){
              grid_dia <- grid.arrange(dia_nPM1, dia_nPM2, dia_nPM4, dia_nPM10, 
                                       ncol=2, nrow=2, top="Concentração Diária dos Particulados")
              pathDest2 <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\gf_concent\\"
              fileDest2 <- paste(pathDest2,  "Dia_", dataset$dia_mes_ano[1], ".png", sep = "")
              png(filename = fileDest2, width = 1280, height = 720, units = 'px')
              plot(grid_dia)
              dev.off()
          }
        }        
        
        # GRAFICOS COM A MASSA E CONCENT. DIARIA DE PARTICULADOS
        if (OP_DIARIO == 3){
          
          ##############################################################################
          #########  GRAFICOS COM A MASSA E CONCENT. DIARIA DE PARTICULADOS ############
          
          dia_PM1 <- ggplot(data = datasetDiario, aes(x = hora_minuto)) +
            geom_point( aes(y=massaPM1*1, group = 1), colour = 'blue', size = 1) +
            geom_line( aes(y=massaPM1*1, group = 1), colour = 'blue', size = 1) +
            geom_point( aes(y=numPM1/10, group = 1), colour = 'red', size = 1) +
            geom_line( aes(y=numPM1/10, group = 1), colour = 'red', size = 1) +
            scale_y_continuous(
              name = "MASSA - [u/cm³]",
              sec.axis = sec_axis(~.*10, name = "Concentracao - [#/cm³]")
            ) + theme_ipsum() + 
            theme(
              axis.title.y = element_text(color = 'blue', size=13),
              axis.title.y.right = element_text(color = 'red', size=13)
            ) + ggtitle("Massa [u/cm³] e Concentração [#/cm³] - PM1.0") +
            theme(axis.text.x = element_text(angle = 60))
          
          dia_PM2 <- ggplot(data = datasetDiario, aes(x = hora_minuto)) +
            geom_point( aes(y=massaPM2*1, group = 1), colour = 'blue', size = 1) +
            geom_line( aes(y=massaPM2*1, group = 1), colour = 'blue', size = 1) +
            geom_point( aes(y=numPM2/10, group = 1), colour = 'red', size = 1) +
            geom_line( aes(y=numPM2/10, group = 1), colour = 'red', size = 1) +
            scale_y_continuous(
              name = "MASSA [u/cm³]",
              sec.axis = sec_axis(~.*10, name = "Concentracao - [#/cm³]")
            ) + theme_ipsum() + 
            theme(
              axis.title.y = element_text(color = 'blue', size=13),
              axis.title.y.right = element_text(color = 'red', size=13)
            ) + ggtitle("Massa [u/cm³] e Concentração [#/cm³] - PM2.5") +
            theme(axis.text.x = element_text(angle = 60))
          
          dia_PM4 <- ggplot(data = datasetDiario, aes(x = hora_minuto)) +
            geom_point( aes(y=massaPM4*1, group = 1), colour = 'blue', size = 1) +
            geom_line( aes(y=massaPM4*1, group = 1), colour = 'blue', size = 1) +
            geom_point( aes(y=numPM4/10, group = 1), colour = 'red', size = 1) +
            geom_line( aes(y=numPM4/10, group = 1), colour = 'red', size = 1) +
            scale_y_continuous(
              name = "MASSA [u/cm³]",
              sec.axis = sec_axis(~.*10, name = "Concentracao [#/cm³]")
            ) + theme_ipsum() + 
            theme(
              axis.title.y = element_text(color = 'blue', size=13),
              axis.title.y.right = element_text(color = 'red', size=13)
            ) + ggtitle("Massa [u/cm³] e Concentração [#/cm³] - PM4.0") +
            theme(axis.text.x = element_text(angle = 60))
          
          dia_PM10 <- ggplot(data = datasetDiario, aes(x = hora_minuto)) +
            geom_point( aes(y=massaPM10*1, group = 1), colour = 'blue', size = 1) +
            geom_line( aes(y=massaPM10*1, group = 1), colour = 'blue', size = 1) +
            geom_point( aes(y=numPM10/10, group = 1), colour = 'red', size = 1) +
            geom_line( aes(y=numPM10/10, group = 1), colour = 'red', size = 1) +
            scale_y_continuous(
              name = "MASSA [u/cm³]",
              sec.axis = sec_axis(~.*10, name = "Concentracao [#/cm³]")
            ) + theme_ipsum() + 
            theme(
              axis.title.y = element_text(color = 'blue', size=13),
              axis.title.y.right = element_text(color = 'red', size=13)
            ) + ggtitle("Massa [u/cm³] e Concentração [#/cm³] - PM10.0") +
            theme(axis.text.x = element_text(angle = 60))
          
          #if (imprimir == 1){
          if (1 == 1){
            grid_dia <- grid.arrange(dia_PM1, dia_PM2, dia_PM4, dia_PM10, 
                                     ncol=2, nrow=2, top="Massa e Concentração Diária dos Particulados")
            pathDest2 <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\gf_massaConcent\\"
            fileDest2 <- paste(pathDest2,  "Dia_", dataset$dia_mes_ano[1], ".png", sep = "")
            png(filename = fileDest2, width = 1550, height = 720, units = 'px')
            plot(grid_dia)
            dev.off()
          }
        }   
        
        # calcula a massa acumulada com o decorrer dos dias
        if (OP_M_ACC == 1){
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
          
          m2 = Vd2 * ListaMassaMedia[length(ListaMassaMedia)] * 10^(-6) * cosd(theta) * t_sec # -,,,
          
          datasetMacc2_aux <- rbind(datasetMacc2_aux, list(dataset$dia_mes_ano[1], Vd1, Pd, Nloss, 
                                                         Vd2, m2, x_gauss), deparse.level = 1)
       
          
        }
    }

}

# salvar dataset e grafico  do calculo da massa acumulada
if (OP_M_ACC == 1){
#  datasetMacc_aux <- datasetMacc_aux[-c(1),]
  write_csv(datasetMacc_aux,'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\m_acc\\DatasetMassaAcc.csv')
  
#  datasetMacc2_aux <- datasetMacc2_aux[-c(1),]
  write.csv(datasetMacc2_aux,'D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\m_acc\\DatasetMassaAcc2.csv')
  
  datasetMacc_aux$newDate <- as.Date.character(datasetMacc_aux$dia, "%Y%m%d")
  datasetMacc_aux <- datasetMacc_aux[-c(1),]
# Grafico da Massa Acumulada LOG
  ###################################
  pathDest <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\m_acc\\"
  fileDest <- paste(pathDest,  "Graf_MassaACC_logTESTE.png", sep = "")
  png(filename = fileDest, width = 720, height = 360, units = 'px')
  
  gf_MassaAcc <- ggplot() +
    geom_point(data = datasetMacc_aux, aes(newDate, m, group = 1)) +
    geom_line(data = datasetMacc_aux, aes(newDate, m, group = 1)) +
    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
    scale_y_log10()+
    labs(x = "Dias", y = "Massa da Partícula Acumulada [ug/cm³]") +
    ggtitle("Massa Diária") +
    theme(axis.text.x = element_text(angle = 60))
  
  plot(gf_MassaAcc)

  dev.off()  
  
  windows()
  
# Grafico da Massa Acumulada Linear
###################################  
  gf_MassaAcc <- ggplot() +
    geom_point(data = datasetMacc_aux, aes(newDate, m, group = 1)) +
    geom_line(data = datasetMacc_aux, aes(newDate, m, group = 1)) +
    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
    labs(x = "Dia", y = "Massa da Partícula Acumulada [ug/cm³]") +
    ggtitle("Massa Diária") +
    theme(axis.text.x = element_text(angle = 60))
  
  
  pathDest <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\m_acc\\"
  fileDest <- paste(pathDest,  "Graf_MassaACC_linear.png", sep = "")
  png(filename = fileDest, width = 1550, height = 720, units = 'px')
  plot(gf_MassaAcc)
  dev.off()  
  
}



########################################################################################
#####         TRECHO PARA IMPRESSAO  DE TODOS OS DIAS NO MESMO GRAFICO        ##########
########################################################################################

todosDiasGroup <- dplyr::group_by(datasetAux, dia_mes_ano)

todosDiasSummarise <- dplyr::summarise(todosDiasGroup, 
                       mPM1acc = round(sum(massaPM1, na.rm=TRUE), digits=2),
                       cPM1acc = round(sum(numPM1, na.rm=TRUE), digits = 2),
#                      mediaPM1 = round(mean(numPM1, na.rm=TRUE), digits = 2),
                                       
                       mPM2acc = round(sum(massaPM2, na.rm=TRUE), digits=2),
                       cPM2acc = round(sum(numPM2, na.rm=TRUE), digits = 2),
#                      mediaPM2 = round(mean(numPM2, na.rm=TRUE), digits = 2),
                                       
                       mPM4acc = round(sum(massaPM4, na.rm=TRUE), digits=2),
                       cPM4acc = round(sum(numPM4, na.rm=TRUE), digits = 2),
#                      mediaPM4 = round(mean(numPM4, na.rm=TRUE), digits = 2),
                                       
                       mPM10acc = round(sum(massaPM10, na.rm=TRUE), digits=2),
                       cPM10acc = round(sum(numPM10, na.rm=TRUE), digits = 2),
#                      mediaPM10 = round(mean(numPM10, na.rm=TRUE), digits = 2),
                    
                       n = dplyr::n())#,

todosDiasSummarise$newDate <- as.Date.character(todosDiasSummarise$dia_mes_ano, "%Y%m%d")


# Gráfico de todos os dias dos 4 Particulados para Massa
if ( OP_GERAL == 1 ){
 
  gf_PM1 <- ggplot() +
    geom_point(data = todosDiasSummarise, aes(newDate, mPM1acc, group = 1)) +
    geom_line(data = todosDiasSummarise, aes(newDate, mPM1acc, group = 1)) +
    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
    labs(x = "Dia", y = "Massa da Partícula [ug/cm³]") +
    ggtitle("Massa Diária - PM1.0") +
    theme(axis.text.x = element_text(angle = 60))
  
  gf_PM2 <- ggplot() +
    geom_point(data = todosDiasSummarise, aes(newDate, mPM2acc, group = 2), colour = 'darkgreen', size = 1) +
    geom_line(data = todosDiasSummarise, aes(newDate, mPM2acc, group = 2), colour = 'darkgreen') +
    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
    labs(x = "Dia", y = "Massa da Partícula [ug/cm³]") +
    ggtitle("Massa Diária - PM2.5") +
    theme(axis.text.x = element_text(angle = 60))
  
  gf_PM4 <- ggplot() +
    geom_point(data = todosDiasSummarise, aes(newDate, mPM4acc, group = 3), colour = 'red', size = 1) +
    geom_line(data = todosDiasSummarise, aes(newDate, mPM4acc, group = 4), colour = 'red') +
    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
    labs(x = "Dia", y = "Massa da Partícula [ug/cm³]") +
    ggtitle("Massa Diária - PM4.0") +
    theme(axis.text.x = element_text(angle = 60))
  
  gf_PM10 <- ggplot() +
    geom_point(data = todosDiasSummarise, aes(newDate, mPM10acc, group = 4), colour = 'blue', size = 1) +
    geom_line(data = todosDiasSummarise, aes(newDate, mPM10acc, group = 4), colour = 'blue') +
    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
    labs(x = "Dia", y = "Massa da Partícula [ug/cm³]") +
    ggtitle("Massa Diária - PM10.0") +
    theme(axis.text.x = element_text(angle = 60)) 
  
  grid <- grid.arrange(gf_PM1, gf_PM2, gf_PM4, gf_PM10, ncol=2, nrow=2, top="Massa Diária dos Particulados")
  
  break 
  pathDest <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\gf_massa\\"
  fileDest <- paste(pathDest,  "aA_TodosDias.png", sep = "")
#  png(filename = fileDest, width = 1280, height = 720, units = 'px')
  png(filename = fileDest, width = 1550, height = 720, units = 'px')
  plot(grid)
  dev.off()  
  
  
  ########################################################################################
}

# Gráfico de todos os dias dos 4 Particulados para Concentração
if ( OP_GERAL == 2 ){
  gf_cPM1 <- ggplot() +
    geom_point(data = todosDiasSummarise, aes(newDate, cPM1acc, group = 1)) +
    geom_line(data = todosDiasSummarise, aes(newDate, cPM1acc, group = 1)) +
    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
    labs(x = "Dia", y = "Concentração da Partícula [ug/cm³]") +
    ggtitle("Concentração Diária - PM1.0") +
    theme(axis.text.x = element_text(angle = 60))
  
  gf_cPM2 <- ggplot() +
    geom_point(data = todosDiasSummarise, aes(newDate, cPM2acc, group = 2), colour = 'darkgreen', size = 1) +
    geom_line(data = todosDiasSummarise, aes(newDate, cPM2acc, group = 2), colour = 'darkgreen') +
    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
    labs(x = "Dia", y = "Concentração da Partícula [ug/cm³]") +
    ggtitle("Concentração Diária - PM2.5") +
    theme(axis.text.x = element_text(angle = 60))
  
  gf_cPM4 <- ggplot() +
    geom_point(data = todosDiasSummarise, aes(newDate, cPM4acc, group = 3), colour = 'red', size = 1) +
    geom_line(data = todosDiasSummarise, aes(newDate, cPM4acc, group = 4), colour = 'red') +
    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
    labs(x = "Dia", y = "Concentração da Partícula [ug/cm³]") +
    ggtitle("Concentração Diária - PM4.0") +
    theme(axis.text.x = element_text(angle = 60))
  
  gf_cPM10 <- ggplot() +
    geom_point(data = todosDiasSummarise, aes(newDate, cPM10acc, group = 4), colour = 'blue', size = 1) +
    geom_line(data = todosDiasSummarise, aes(newDate, cPM10acc, group = 4), colour = 'blue') +
    scale_x_date(labels = date_format("%d %b"), breaks = date_breaks("week")) +
    labs(x = "Dia", y = "Concentração da Partícula [ug/cm³]") +
    ggtitle("Concentração Diária - PM10.0") +
    theme(axis.text.x = element_text(angle = 60)) 
  
  grid <- grid.arrange(gf_cPM1, gf_cPM2, gf_cPM4, gf_cPM10, 
                       ncol=2, nrow=2, top="Concentração Diária dos Particulados")
  
  pathDestc <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\gf_concent\\"
  fileDestc <- paste(pathDestc,  "aA_TodosDias.png", sep = "")
  png(filename = fileDestc, width = 1550, height = 720, units = 'px')
  plot(grid)
  dev.off()

  ########################################################################################
}

# Gráfico de todos os dias dos 4 Particulados para Massa e Concentração
if ( OP_GERAL == 3 ){
  
  pathDestc <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\gf_massaConcent\\bbbb\\"
  
  gf_PM1 <- ggplot(data = todosDiasSummarise, aes(newDate)) +
    geom_point( aes(y=mPM1acc*1, group = 1), colour = 'blue', size = 1) +
    geom_line ( aes(y=mPM1acc*1, group = 1), colour = 'blue') +
    geom_point( aes(y=cPM1acc/10, group = 1), colour = 'red', size = 1) +
    geom_line ( aes(y=cPM1acc/10, group = 1), colour = 'red') +
    scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("week")) +
    scale_y_continuous(
      name = "MASSA - [u/cm³]",
      sec.axis = sec_axis(~.*10, name = "Concentracao - [#/cm³]")
    ) + theme_ipsum() + 
    theme(
      axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),
      axis.title.y.right = element_text(hjust = 0.5, color = 'red', size=13)
    ) + ggtitle("Massa [u/cm³] e Concentração [#/cm³] - PM1.0") +
    theme(axis.text.x = element_text(angle = 60)) +
    theme(text = element_text(size=20), axis.text.x = element_text(angle = 45))
  
  
  fileDestc <- paste(pathDestc,  "ConcTodosDias_PM1.png", sep = "")
  png(filename = fileDestc, width = 1100, height = 600, units = 'px')  
  plot(gf_PM1)
  dev.off()

  
  
  gf_PM2 <- ggplot(data = todosDiasSummarise, aes(newDate)) +
    geom_point( aes(y=mPM2acc*1, group = 1), colour = 'blue', size = 1) +
    geom_line ( aes(y=mPM2acc*1, group = 1), colour = 'blue') +
    geom_point( aes(y=cPM2acc/1, group = 1), colour = 'red', size = 1) +
    geom_line ( aes(y=cPM2acc/1, group = 1), colour = 'red') +
    scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("week")) +
    scale_y_continuous(
      name = "MASSA - [u/cm³]",
      sec.axis = sec_axis(~.*1, name = "Concentracao - [#/cm³]")
    ) + theme_ipsum() + 
    theme(
      axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),
      axis.title.y.right = element_text(hjust = 0.5, color = 'red', size=13)
    ) + ggtitle("Massa [u/cm³] e Concentração [#/cm³] - PM2.5") +
    theme(axis.text.x = element_text(angle = 60)) +
    theme(text = element_text(size=20), axis.text.x = element_text(angle = 45))
  
  
  fileDestc <- paste(pathDestc, "ConcTodosDias_PM2.png", sep = "")
  png(filename = fileDestc, width = 1100, height = 600, units = 'px')  
  plot(gf_PM2)
  dev.off()
  
  gf_PM4 <- ggplot(data = todosDiasSummarise, aes(newDate)) +
    geom_point( aes(y=mPM4acc*1, group = 1), colour = 'blue', size = 1) +
    geom_line ( aes(y=mPM4acc*1, group = 1), colour = 'blue') +
    geom_point( aes(y=cPM4acc/1, group = 1), colour = 'red', size = 1) +
    geom_line ( aes(y=cPM4acc/1, group = 1), colour = 'red') +
    scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("week")) +
    scale_y_continuous(
      name = "MASSA - [ug/cm³]",
      sec.axis = sec_axis(~.*1, name = "Concentracao - [#/cm³]")
    ) + theme_ipsum() + 
    theme(
      axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),
      axis.title.y.right = element_text(hjust = 0.5, color = 'red', size=13)
    ) + ggtitle("Massa [ug/cm³] e Concentração [#/cm³] - PM4.0") +
    theme(axis.text.x = element_text(angle = 60)) +
    theme(text = element_text(size=20), axis.text.x = element_text(angle = 45))
  
  fileDestc <- paste(pathDestc,  "ConcTodosDias_PM4.png", sep = "")
  png(filename = fileDestc, width = 1100, height = 600, units = 'px')  
  plot(gf_PM4)
  dev.off()
  
  
  gf_PM10 <- ggplot(data = todosDiasSummarise, aes(newDate)) +
    geom_point( aes(y=mPM10acc*1, group = 1), colour = 'blue', size = 1) +
    geom_line ( aes(y=mPM10acc*1, group = 1), colour = 'blue') +
    geom_point( aes(y=cPM10acc/1, group = 1), colour = 'red', size = 1) +
    geom_line ( aes(y=cPM10acc/1, group = 1), colour = 'red') +
    scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("week")) +
    scale_y_continuous(
      name = "MASSA - [ug/cm³]",
      sec.axis = sec_axis(~.*1, name = "Concentracao - [#/cm³]")
    ) + theme_ipsum() + 
    theme(
      axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),
      axis.title.y.right = element_text(hjust = 0.5, color = 'red', size=13)
    ) + ggtitle("Massa [ug/cm³] e Concentração [#/cm³] - PM10.0") +
    theme(axis.text.x = element_text(angle = 60)) +
    theme(text = element_text(size=20), axis.text.x = element_text(angle = 45))
  
  fileDestc <- paste(pathDestc,  "ConcTodosDias_PM10.png", sep = "")
  png(filename = fileDestc, width = 1100, height = 600, units = 'px')  
  plot(gf_PM10)
  dev.off()
  
  
  grid <- grid.arrange(gf_PM1, gf_PM2, gf_PM4, gf_PM10, 
                       ncol=2, nrow=2, top="Concentração Diária dos Particulados")
  
  
  fileDestc <- paste(pathDestc,  "aA_TodosDias.png", sep = "")
  png(filename = fileDestc, width = 1550, height = 720, units = 'px')
  plot(grid)
  dev.off()
  
  ########################################################################################
}

# A FAZER: Mostrar M_ACC x SPS30 
if ( OP_GERAL == 4 ){
  
# D:\github\Tabelas_DynamoDB\csv_para_analise\tabelas_teste\09_combinacaofinal\reduzidas  
  
  
  ########################################################################################
}


# Grafico para Precipitação Pluviométrica apenas
if ( OP_CHUVA == 1 ){
  
  caminhoChuva <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\preciptacao_inmet\\"
  
  cam_chuva <- setwd(caminhoChuva)
  cam_chuva <- setwd(caminhoChuva)
  csvChuva <- list.files(pattern = "*.csv")
  chuva <- paste(cam_chuva,  "/", csvChuva, sep = "")
  
  datasetChuva <- readr::read_csv(csvChuva, col_types = cols(precipitacao = col_character()))
  datasetChuva$newDate <- parse_date_time(datasetChuva$data, orders = c("ymd", "dmy", "mdy"))
#  datasetChuva$newDate2 <- as.Date(datasetChuva$newDate)
#  datasetChuva$newDate2 <- NULL
  datasetChuva$precipitacao <- as.numeric(datasetChuva$precipitacao)
  
  
  
  datasetMacc_aux$newDate <- parse_date_time(datasetMacc_aux$dia, orders = c("ymd", "dmy", "mdy"))
#  datasetMacc_aux$newDate2 <- as.Date(datasetMacc_aux$newDate)
#  datasetMacc_aux$newDate2 <- NULL
  
  
  
  datasetTeste3 <- merge(datasetMacc_aux, datasetChuva, by.x = "newDate", 
                        by.y = "newDate", all.x = FALSE, all.y = TRUE)
  
#  datasetTeste3 = datasetTeste3[-c(1),]        # tirar as 3 primeiras linhas, executar 3x
  
  datasetTeste3$dia <- NULL
  datasetTeste3$data <- NULL
  datasetTeste3$Vd1 <- NULL
  datasetTeste3$Vd2 <- NULL
  datasetTeste3$Nloss <- NULL
  datasetTeste3$Pd <- NULL
  datasetTeste3$x_gauss <- NULL
  
  linhas = nrow(datasetTeste3)
  
  for(aux in 1:linhas){ 
    
    if(is.na(datasetTeste3$m[aux])){
      datasetTeste3$m[aux] = datasetTeste3$m[aux-1]
    }
   
     
  }
    
  
  
  
  # impressao da chuva com grafico de segmentos
  if (OP_CHUVA_T == 1){
  gf_chuva <- ggplot(data = datasetChuva, aes(x = newDate2, y=precipitacao)) + 
    geom_point(colour='blue' ) +
    geom_segment(aes(x=newDate2, xend=newDate2, y=0, yend=precipitacao), colour='blue', size = 1) +
    scale_y_continuous(breaks = 5*0:65) +  
    scale_x_date(date_labels = "%d %b %y", date_breaks = "week") +
    ggtitle("Índice pluviométrico [mm]")  +
    labs(x = "Dia", y = "Preciptação [mm]") +
    theme(axis.text.x = element_text(angle = 60))
  
  pathDestc <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\"
  fileDestc <- paste(pathDestc,  "Chuvas_Segmento.png", sep = "")
  png(filename = fileDestc, width = 1280, height = 720, units = 'px')
  plot(gf_chuva)
  dev.off()
  }
  
  # impressao da chuva com grafico de barras
  if (OP_CHUVA_T == 2){
  gf_chuva2 <- ggplot(datasetChuva, aes(x=newDate2, y=precipitacao))+
    geom_bar(stat = "identity", width = 0.5, colour = 'blue') +
    scale_y_continuous(breaks = 5*0:65) +  
    scale_x_date(date_labels = "%d %b %y ", date_breaks = "week") +
    ggtitle("Índice pluviométrico [mm]")  +
    labs(x = "Dia", y = "Preciptação [mm]") +
    theme(axis.text.x = element_text(angle = 60))
  
  pathDestc <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\"
  fileDestc <- paste(pathDestc,  "Chuvas_Barra.png", sep = "")
  png(filename = fileDestc, width = 1280, height = 720, units = 'px')
  plot(gf_chuva2)
  dev.off()
  }
  

}

if ( OP_CHUVA == 2 ){
  
  caminhoChuva <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\preciptacao_inmet\\"
  
  cam_chuva <- setwd(caminhoChuva)
  cam_chuva <- setwd(caminhoChuva)
  csvChuva <- list.files(pattern = "*.csv")
  chuva <- paste(cam_chuva,  "/", csvChuva, sep = "")
  
  datasetChuva <- readr::read_csv(csvChuva, col_types = cols(precipitacao = col_character()))
  datasetChuva$newDate <- parse_date_time(datasetChuva$data, orders = c("ymd", "dmy", "mdy"))
  datasetChuva$newDate2 <- as.Date(datasetChuva$newDate)
  datasetChuva$precipitacao <- as.numeric(datasetChuva$precipitacao)
  
  datasetMerge <- merge(todosDiasSummarise, datasetChuva, by.x = "newDate", 
                   by.y = "newDate2", all.x = TRUE, all.y = FALSE)
  
  gf_conc_pluv <- ggplot(data = datasetMerge, aes(x = newDate, y = precipitacao/2)) + #start plot by by plotting bars
    geom_bar(stat = "identity", width = 0.3, colour = 'blue') + 
    geom_line(data = datasetMerge, aes(x = newDate, y = cPM1acc/10000, group = 1),  
              size = 1, colour = 'red', inherit.aes = FALSE) +
    geom_point(data = datasetMerge, aes(x = newDate, y = cPM1acc/10000, group = 1),  
              size = 1.5, colour = 'red', inherit.aes = FALSE) +
    
    scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("week")) +
    
    scale_y_continuous( name = "Concentração PM1 - [# x 10^5/cm³]",
                       sec.axis = sec_axis(~.*2, name = "Precipitação [mm]"))+
    theme_ipsum() + 
    theme(
      axis.title.y = element_text(color = 'red', size=20, hjust = .5),
      axis.title.y.right = element_text(color = 'blue', size=20, hjust = .5, angle=270)
    ) + ggtitle("Concentração - [#/cm³] e Preciptação Pluviométrica [mm]") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle = 60))
  
  pathDest <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\m_acc\\"
  fileDest <- paste(pathDest,  "Concent_Chuva.png", sep = "")
  #  png(filename = fileDest, width = 1280, height = 720, units = 'px')
  png(filename = fileDest, width = 1550, height = 720, units = 'px')
  plot(gf_conc_pluv)
  dev.off()  

}


if ( OP_CHUVA == 3 ){
  
  caminhoChuva <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\preciptacao_inmet\\"
  
  cam_chuva <- setwd(caminhoChuva)
  cam_chuva <- setwd(caminhoChuva)
  csvChuva <- list.files(pattern = "*.csv")
  chuva <- paste(cam_chuva,  "/", csvChuva, sep = "")
  
  datasetChuva <- readr::read_csv(csvChuva, col_types = cols(precipitacao = col_character()))
  datasetChuva$newDate <- parse_date_time(datasetChuva$data, orders = c("ymd", "dmy", "mdy"))
  datasetChuva$newDate2 <- as.Date(datasetChuva$newDate)
  datasetChuva$precipitacao <- as.numeric(datasetChuva$precipitacao)
  
  
  datasetMerge <- merge(x=datasetMacc_aux, y=datasetChuva, by.x = "newDate", by.y = "newDate2", all.x = TRUE, all.y = FALSE)

  datasetMerge$m2 <- log(datasetMerge$m)
# GRAFICO MASSA ACUMULADA x PRECIPITACAO - ESCALA LOGARITMICA
################################################################
  gf_conc_pluv <- ggplot(data = datasetMerge, aes(x = newDate, y = precipitacao)) + #start plot by by plotting bars
    geom_bar(stat = "identity", width = 0.3, colour = 'blue') + 
    geom_line(data = datasetMerge, aes(x = newDate, y = m*1, group = 1), 
              size = 0.5, colour = 'red', inherit.aes = FALSE) +
    geom_point(data = datasetMerge, aes(x = newDate, y = m*1, group = 1),  
               size = 0.5, colour = 'red', inherit.aes = FALSE) +
    scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("week")) +

#    scale_y_continuous(name = "Precipitação [mm]", trans = "identity",
#               sec.axis = sec_axis (trans = "log10", name = "Massa Acumulada [ug/cm²]")) +   
    theme_ipsum() +
    scale_y_log10(limits = c(0.01, 50),
      breaks = c(0.1, 0.25, 0.5, 0.8, 1, 5, 10, 15, 20, 30, 40, 50)) + 
    theme(
      axis.title.y = element_text(color = 'blue', size=20, hjust = .5),
      axis.title.y.right = element_text(color = 'red', size=20, hjust = .5, angle=270)) + 
    ggtitle("Massa Acumulada [ug/cm] e Preciptação Pluviométrica [mm]") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle = 60))
  
   plot(gf_conc_pluv)
  
  
  pathDest <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\m_acc\\"
  fileDest <- paste(pathDest,  "MassaAcumulada_ChuvaLOG2.png", sep = "")
  #  png(filename = fileDest, width = 1280, height = 720, units = 'px')
  png(filename = fileDest, width = 1550, height = 720, units = 'px')
  plot(gf_conc_pluv)
  dev.off()  

# GRAFICO MASSA ACUMULADA x PRECIPITACAO - ESCALA LINEAR
################################################################

  gf_conc_pluv <- ggplot(data = datasetMerge, aes(x = newDate, y = precipitacao/10)) + #start plot by by plotting bars
    geom_bar(stat = "identity", width = 0.3, colour = 'blue') + 
    geom_line(data = datasetMerge, aes(x = newDate, y = m*1, group = 1), size = 0.5, colour = 'red', inherit.aes = FALSE) +
    geom_point(data = datasetMerge, aes(x = newDate, y = m*1, group = 1),  size = 0.5, colour = 'red', inherit.aes = FALSE) +
    scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("week")) +
#    scale_y_continuous(name = "Precipitação [mm]", sec.axis = sec_axis (~./10, name = "Massa Acumulada"))+
#scale_y_continuous(name = "Massa Acumulada", 
#                       sec.axis = sec_axis (~./25, name = "Precipitação [mm]", breaks = seq(0,25,1)))+

    scale_y_continuous(name = "Massa Acumulada", trans = "identity",
                        sec.axis = sec_axis (trans = "log10", name = "Precipitação [mm]"))+
    
    theme_ipsum() +
    theme(
      axis.title.y = element_text(color = 'blue', size=20, hjust = .5),
      axis.title.y.right = element_text(color = 'red', size=20, hjust = .5, angle=270)) + 
    ggtitle("Massa Acumulada e Preciptação Pluviométrica [mm]") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle = 60))
  
  plot(gf_conc_pluv)
  
  
  pathDest <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\m_acc\\"
  fileDest <- paste(pathDest,  "MassaAcumulada_ChuvaLINEAR.png", sep = "")
  #  png(filename = fileDest, width = 1280, height = 720, units = 'px')
  png(filename = fileDest, width = 1550, height = 720, units = 'px')
  plot(gf_conc_pluv)
  dev.off()    
  
  
}


# Grafico para Precipitação Pluviométrica e Particulados
if ( OP_M_CHUVA == 1 ){
  
  caminhoChuva <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\tabelas_teste\\preciptacao_inmet\\"
  
  cam_chuva <- setwd(caminhoChuva)
  cam_chuva <- setwd(caminhoChuva)
  csvChuva <- list.files(pattern = "*.csv")
  chuva <- paste(cam_chuva,  "/", csvChuva, sep = "")
  
  datasetChuva <- readr::read_csv(csvChuva, col_types = cols(precipitacao = col_character()))
  datasetChuva$newDate <- parse_date_time(datasetChuva$data, orders = c("ymd", "dmy", "mdy"))
  datasetChuva$newDate2 <- as.Date(datasetChuva$newDate)
  datasetChuva$precipitacao <- as.numeric(datasetChuva$precipitacao)
  
  # impressao da chuva com grafico de segmentos
  if (chuva == 1){
    gf_chuva <- ggplot(data = datasetChuva, aes(x = newDate2, y=precipitacao)) + 
      geom_point(colour='blue' ) +
      geom_segment(aes(x=newDate2, xend=newDate2, y=0, yend=precipitacao), colour='blue', size = 1) +
      scale_y_continuous(breaks = 5*0:65) +  
      scale_x_date(date_labels = "%d %b %y", date_breaks = "week") +
      ggtitle("Índice pluviométrico [mm]")  +
      labs(x = "Dia", y = "Preciptação [mm]") +
      theme(axis.text.x = element_text(angle = 60))
    
    pathDestc <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\"
    fileDestc <- paste(pathDestc,  "Chuvas_Segmento.png", sep = "")
    png(filename = fileDestc, width = 1280, height = 720, units = 'px')
    plot(gf_chuva)
    dev.off()
    
  }
  
  # impressao da chuva com grafico de barras
  if (chuva == 2){
    gf_chuva2 <- ggplot(datasetChuva, aes(x=newDate2, y=precipitacao))+
      geom_bar(stat = "identity", width = 0.5, colour = 'blue') +
      scale_y_continuous(breaks = 5*0:65) +  
      scale_x_date(date_labels = "%d %b %y ", date_breaks = "week") +
      ggtitle("Índice pluviométrico [mm]")  +
      labs(x = "Dia", y = "Preciptação [mm]") +
      theme(axis.text.x = element_text(angle = 60))
    
    pathDestc <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\"
    fileDestc <- paste(pathDestc,  "Chuvas_Barra.png", sep = "")
    png(filename = fileDestc, width = 1280, height = 720, units = 'px')
    
    windows()
    plot(gf_chuva2)
    
    dev.off()
  }
  
  
  if (chuva == 3){
    
    datasetMerge2 <- merge(x = datasetChuva, y = datasetMacc_aux, by.x = 'newDate2', 
                           by.y = 'newDate', all.x = TRUE, all.y = FALSE)
    
#    datasetMacc2_aux <- datasetMacc2_aux[-c(1),]
#    write.csv(datasetMerge2,'C:\\Users\\Guilherme\\Desktop\\Nova pasta\\testeExcel.csv')
    
    
    df3 = datasetMerge2[,!(names(datasetMerge2) %in% c("data","newDate", "dia", "Nloss", "Vd1", "Pd", "Vd2", "x_gauss"))]
    df3$m = df3$m*20
    df3 <- melt(data = df3, id.vars = "newDate2", measure.vars = c("precipitacao", "m"))
    
    pathDestc <- "D:\\github\\Tabelas_DynamoDB\\csv_para_analise\\zz_novos_testes\\"
    fileDestc <- paste(pathDestc,  "Chuva_MACC1.png", sep = "")
    png(filename = fileDestc, width = 1280, height = 720, units = 'px')
    
    gf_chuva3 <- ggplot(data = df3 , aes(x=newDate2, y = value, fill = variable) ) + 
      geom_bar(stat="identity", position="dodge") +
      scale_y_continuous(breaks = 5*0:60, name = "Valores") +
      scale_x_date(name = "Dias Amostrados", labels = date_format("%d %b"), breaks = date_breaks("week"))+
      theme(
        axis.title.y = element_text(hjust = 0.5, color = 'blue', size=13),
    #    axis.title.y.right = element_text(hjust = 0.5, color = 'red', size=13)
      ) + 
      theme_ipsum() + ggtitle("Massa Acumulada [ug/cm³] e Precipitação Pluviométrica Diária [mm/dia]") +
      theme(axis.text.x = element_text(angle = 60))
    
    
    plot(gf_chuva3)
    windows()

    plot(gf_chuva3)
    dev.off()
    
  }
  
  
  
}




