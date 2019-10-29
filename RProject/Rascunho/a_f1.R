require(stringr)
library(lubridate)
require(dplyr)

#pathrepo <- "/home/kymberlim/Documentos/ProjetoSolar/repositorio/monitoramento-ufms"
pathrepo <- "~/Projeto/monitoramento-ufms"
regex.inversor.path <- regex("InversorFronius")
regex.inversor.file <- regex(".*Relatório_diário_.*.csv")
columns.inversor <- c("timestamp","I_CA", "I_CC", "V_CA", "V_CC", "E_CA")

#regex.estacao.path <- regex("WeatherHawk$")
#columns.estacao <- c("timestamp","dir_vento", "vel_vento", "raj_vento", "umidade", "temp_in", "temp", "raw_barom", "chuva_tot", "rad_sol", "p_orvalho", "barom_sl", "rad_sol_avg", "vel_vento_max", "raj_vento_max", "umidade_max", "temp_in_max", "temp_max", "raw_barom_max", "chuva_tot_max", "rad_sol_max", "barom_sl_max", "vel_vento_min", "raj_vento_min", "umidade_min", "temp_in_min", "temp_min", "raw_barom_min", "chuva_tot_min", "rad_sol_min", "barom_sl_min", "moon_phase", "data")
#columns.estacao.tonumeric <- c("dir_vento", "vel_vento", "raj_vento", "umidade", "temp_in", "temp", "raw_barom", "chuva_tot", "rad_sol", "p_orvalho", "barom_sl", "rad_sol_avg", "vel_vento_max", "raj_vento_max", "umidade_max", "temp_in_max", "temp_max", "raw_barom_max", "chuva_tot_max", "rad_sol_max", "barom_sl_max", "vel_vento_min", "raj_vento_min", "umidade_min", "temp_in_min", "temp_min", "raw_barom_min", "chuva_tot_min", "rad_sol_min", "barom_sl_min", "moon_phase")

filter_data <- strptime("10/11/2018", format = "%d/%m/%Y")

f1 <- function(x){
  if(!is.na(str_extract(x, regex.inversor.path))){
    files <- list.files(x, full.names = TRUE, include.dirs = TRUE, recursive = TRUE, pattern = regex.inversor.file)
    d <- do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE, header = FALSE, skip = 2, na.strings = "n/a")))
    return(d)
  }
  return()
}

f2 <- function(x){
  if(!is.na(str_extract(x, regex.estacao.path))){
    files <- list.files(x, full.names = TRUE, include.dirs = TRUE, recursive = TRUE, pattern = regex(".csv$"))
    files <- files[str_sub(files, -10, -5) >= strftime(filter_data, "%y%m%d")]
    d <- do.call("rbind", lapply(files, f3))
    return(d)
  }
  return()
}

f3 <- function(x){
  d <- read.csv(x, stringsAsFactors = FALSE, header = FALSE, skip = 3, na.strings = "n/a")
  d$data = str_sub(x, -10, -5)
  return(d)
}

