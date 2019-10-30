library(dplyr)
library(magrittr)
library(readr)
library(tidyverse)

fileDia <- 20191003
csvPath <- "D:/github/Tabelas_DynamoDB/merge/daily/Inv_Est_Merge_"
csvFile <- paste(csvPath, fileDia, ".csv", sep = "")
#View(csvFile)

dataset <- readr::read_csv(csvFile, col_types = cols(hora_minuto = col_character()))


surveys_complete = as.data.frame(dataset)
#View(dataset)


ggplot(data = surveys_complete, 
       mapping = aes(x = surveys_complete$hora_minuto, y = surveys_complete$temperatura,
                                                           fill=vento_dir)) + 
                         geom_bar(position=position_dodge(), stat="identity")

