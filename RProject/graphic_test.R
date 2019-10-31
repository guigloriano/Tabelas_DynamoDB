library(dplyr)
library(magrittr)
library(readr)
library(tidyverse)
library(ggplot2)


fileDia <- 20191003
csvPath <- "D:/github/Tabelas_DynamoDB/merge/daily/Inv_Est_Merge_"
csvFile <- paste(csvPath, fileDia, ".csv", sep = "")
#View(csvFile)

dataset <- readr::read_csv(csvFile, col_types = cols(hora_minuto = col_character()))


surveys_complete = as.data.frame(dataset)
#View(dataset)


ggplot(data = surveys_complete, 
       mapping = aes(x = hora_minuto, y = pm1_massa, group = 1)) + 
                         geom_line(color="grey", size=1) + 
                         geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
                         theme(axis.text.x = element_text(angle = 90)) +
                         labs(x = "Hora", y = "Massa PM1.0") +
                         ggtitle("Distribuição de Massa PM1.0", center)



#ggplot(data = surveys_complete, mapping = aes(x = hora_minuto, y = pm1_massa, fill=vento_dir)) + 
#  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90))