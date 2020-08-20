library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(aweek)
library(stringr)
library(RCurl)
library(car)
library(bbmle)
source("../nowcasting/fct/get.last.date.R")

################################################################################
## leitura da última planilha de nowcasting de casos do Observatório
################################################################################
data.dir <- "../site/dados/municipios/SP/Sao_Paulo/tabelas_nowcasting_para_grafico/"
data <- get.last.date(data.dir)
casos <- read.csv(paste0(data.dir,"nowcasting_diario_covid_", data,".csv"))
srag <- read.csv(paste0(data.dir,"nowcasting_diario_srag_", data,".csv"))


################################################################################
## Censo hospitalar
censo <- read.csv2("../dados/estado_SP/SRAG_hospitalizados/dados/senso_covid/CensoCOVID_Full_Data_data_2020_08_19.csv")
censo$tot.susp.dia <- with(censo, Total.Enfermaria.Suspeito.no.dia..Nao.SUS.+
                                  Total.Enfermaria.Suspeito.no.dia..SUS.+
                                  Total.UTI.Suspeito.no.dia..Nao.SUS. +
                                  Total.UTI.Suspeito.no.dia..SUS.)
