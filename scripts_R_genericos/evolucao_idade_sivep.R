library(dplyr)
library(plyr)
library(zoo)
library(ggplot2)
library(readr)
library(lubridate)
library(NobBS)
library(aweek)
source("../nowcasting/fct/gera.nowcasting.R")
source("../nowcasting/fct/write.notificacoes.data.R")
source("../nowcasting/fct/prepara_dados2.R")
source("../nowcasting/fct/get.last.date.R")
source("../nowcasting/fct/read.sivep.R")
source("../nowcasting/fct/preenche.now.R")


################################################################################
## dados n de notificacoes SEADE
################################################################################
seade <- read.csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/casos_obitos_doencas_preexistentes.csv")
seade.covid <- seade[seade$diagnostico_covid19=="CONFIRMADO",]
seade.covid$data_inicio_sintomas <- as.Date(seade.covid$data_inicio_sintomas, "%m/%d/%Y") 
seade.covid$semana <- date2week(seade.covid$data_inicio_sintomas, numeric=TRUE)
################################################################################
## Primeira etapa: preparacao dos dados: nowcasting acumulado de casos e obitos,
## por data de primeiro sintoma
################################################################################
data.dir <- "../dados/estado_SP/SRAG_hospitalizados/dados/"
dados <- read.sivep(dir = data.dir, escala = "estado", sigla = "SP", geocode = 35, data = get.last.date(data.dir))
dados$semana  <-  date2week(dados$dt_sin_pri, numeric=TRUE)


## plots

png("boxplot_idade_casos%1d.png", width = 600)
ggplot(seade.covid, aes(as.factor(semana), idade)) +
    geom_boxplot() + ggtitle("Estado SP, dados SEADE")

dados %>% filter(pcr_sars2 == 1 | classi_fin ==5) %>%
    ggplot(aes(as.factor(semana), as.integer(nu_idade_n))) +
    geom_boxplot() + ggtitle("Estado SP, SIVEP")
dev.off()
