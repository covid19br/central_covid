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
## Dados SEADE
################################################################################
seade <- read.csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes.csv")
seade.estado <- seade %>%
    filter(nome_drs == "Estado de São Paulo") %>%
    mutate(n
################################################################################
## Censo hospitalar do Estado (dado sigiloso)
################################################################################
censo <- read.csv2("../dados/estado_SP/SRAG_hospitalizados/dados/senso_covid/CensoCOVID_Full_Data_data_2020_08_19.csv")
## Resumo para todo o Estado
censo.zoo <- censo %>%
    filter(Municipio == "SÃO PAULO") %>%
    mutate(data = as.Date(Data.da.Notificacao, "%d/%m/%Y")) %>%
    group_by(data) %>%
    summarise(
        N.susp.Enf = sum(Pacientes.Enfermaria.ou.Retaguarda.PS...Suspeitos, na.rm=TRUE),
        N.susp.UTI = sum(Pacientes.UTI...Suspeitos, na.rm=TRUE),
        N.conf.Enf = sum(Pacientes.Enfermaria.ou.Retaguarda.PS...Confirmados, na.rm=TRUE),
        N.conf.UTI = sum(Pacientes.UTI...Confirmados, na.rm=TRUE),
        N.alta.susp = sum(Altas.hospitalares.de.pacientes.suspeitos.COVID.19..no.periodo.de.00.horas.as.23.horas.e.59.minutos.do.dia.anterior, na.rm=TRUE),
        N.alta.conf = sum(Altas.hospitalares.de.pacientes.confirmados.COVID.19..no.periodo.de.00.horas.as.23.horas.e.59.minutos.do.dia.anterior, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(n.susp = N.susp.Enf + N.susp.UTI,
           n.conf = N.conf.Enf + N.conf.UTI,
           n.casos = n.susp+n.conf,
           n.altas = N.alta.conf + N.alta.susp) %>%
    zoo()
##Ultimos dias
tail(censo.zoo)

################################################################################
## Resumo da opera: o censo mostra o n de internações ativas
################################################################################
summary(censo$Data.da.Notificacao)
