library(dplyr)
library(lubridate)
library(scales)
library(reshape2)
library(ggplot2)

testeDiasOK$dia_mes_ano <- as.Date(testeDiasOK$dia_mes_ano)
#### teste do gráfico de potência usando DIAS no eixo X  ####
p1 = ggplot() + 
  geom_line(data = testeDiasOK, aes(x = dia_mes_ano, y = Predita , group=1), color = "blue", size = 0.5) +
  geom_line(data = testeDiasOK, aes(x = dia_mes_ano, y = Observada , group=2), color = "red", size = 0.5) +
  geom_point(data = testeDiasOK,aes(x = dia_mes_ano, y = Predita , group=1), color = "blue", size = 1) +
  geom_point(data = testeDiasOK,aes(x = dia_mes_ano, y = Observada , group=2), color = "red", size = 1) +
  scale_x_date(name="Dia", labels = date_format("%d %b"), breaks = date_breaks("5 days"), expand = c(0,0)) +
  xlab('Dias') + ylab('Potência') +  theme_bw() + 
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 30))

print(p1)
dev.off()



ver_eqm <- numeric()
ver_eqm <- data.frame((eqm2))
mean(eqm2)  


#### grafico do EQM com a tendencia do erro ####
nome_arquivo <- paste ("eqm2_105", ".png", sep="")
pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\"
fileDest <- paste(pathDest, nome_arquivo, sep = "")
png(filename = fileDest, width = 720, height = 480, units = 'px')


p2 = ggplot() + theme_bw() + 
  geom_line(aes(x = sm, y = eqm2 , group=1), color = "blue", size = 0.6) +
  geom_point(aes(x = sm, y = eqm2 , group=1), color = "blue", size = 1.5) +
  scale_x_continuous(breaks = 5*0:106, expand = c(0.01,0.01)) +
  geom_smooth(aes(x = sm, y = eqm2),method="loess", se = F, linetype = "dashed",
              color = "red") +
#  geom_line(aes(x = sm, y = mean(eqm2) , group=1), color = "black", linetype = "dotted", size = 1) +
  xlab('Dias do Experimento') +
  ylab('EQM') + 
  theme(text = element_text(size=20),
                      axis.text.x = element_text(angle=45, hjust=1))

print(p2)
dev.off()



#### grafico da Dif. Per. Media ####


nome_arquivo <- paste ("dif_perc1_", i+84, ".png", sep="")
pathDest <- "C:\\Users\\Guilherme\\Desktop\\Enviar Ricardo\\blocos_21dias\\"
fileDest <- paste(pathDest, nome_arquivo, sep = "")
png(filename = fileDest, width = 720, height = 480, units = 'px')


new_bar_DPM <- data.frame("dia" = seq(1,105), "DPM" = Dim)
p3 <- ggplot(data = new_bar_DPM , aes(x=dia, y = DPM, color="gray") ) + 
  geom_bar(stat="identity", position="dodge", color="black",) +
  scale_y_continuous(breaks = seq(from = -1.5, to = 1.5, by = 0.1), 
                     name = "%") +
  scale_x_continuous(breaks = 5*0:106, expand = c(0.01,0.01), name = "Dias do Experimento") +
  theme(axis.title.y = element_text(hjust = 0.5, color = 'black', size=13),) + 
  #  theme_ipsum() + 
  ggtitle("Diferença Percentual Média Diária") +  theme_bw() + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45))
plot(p3)


dev.off()
teste_data <- data.frame(new_bar_DPM)

new_bar_DPMabs <- abs(new_bar_DPM)
