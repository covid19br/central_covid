library(dplyr)
library(plyr)
library(zoo)
library(ggplot2)
library(readr)
library(lubridate)
library(NobBS)
source("../nowcasting/fct/gera.nowcasting.R")
source("../nowcasting/fct/write.notificacoes.data.R")
source("../nowcasting/fct/prepara_dados2.R")
source("../nowcasting/fct/get.last.date.R")
source("../nowcasting/fct/read.sivep.R")
source("../nowcasting/fct/beta.cumsum.R") 

################################################################################
## Dados populacionais e do inquerito: Campinas
################################################################################
## Populacao estimada em 2020 por faixa etaria SEADE
## total
Pop.tot <- 1175501
## Até 3 anos
Pop.0.3 <- 53825
## Até dois anos
Pop.0.2 <- Pop.0.3*2/3
## Populacao testada
Npop <- Pop.tot - Pop.0.2
## Inquerito sorologico da SMS Campinas
## https://covid-19.campinas.sp.gov.br/sites/covid-19.campinas.sp.gov.br/files/recomendacoes-tecnicas/I%20Inqu%C3%A9rito%20Soroepidemiol%C3%B3gico%20COVID-19%20Campinas%202020.pdf
inq.data <- as.Date("2020-06-20")
inq.IR <- Npop * 0.0222

################################################################################
## Primeira etapa: preparacao dos dados: nowcasting acumulado de casos e obitos,
## por data de primeiro sintoma
################################################################################
## Leitura dos dados: sivep residentes São Paulo, maiores de 17 anos
data.dir <- "../dados/estado_SP/SRAG_hospitalizados/dados/"
dados <- read.sivep(dir = data.dir, escala = "municipio",
                    geocode = 3509502, data = get.last.date(data.dir)) %>%
    filter(as.integer(nu_idade_n) > 2)
## COVID ##
## Nowcasting de obitos, com data de sintoma como onset
## Nowcasting ate o dia do inquerito
## N de dias do trim
inq.trim <- dados%>%
    filter(evolucao==2 & (pcr_sars2==1|classi_fin==5))%>%
    summarise(max(dt_sin_pri) - inq.data) %>% as.integer()
##nowcasting
covid.ob.now <-  gera.nowcasting(dados, caso = FALSE, tipo = "covid",
                               hospitalizados = FALSE, trim.now = inq.trim, window = 40,
                               obito_sin_pri = TRUE) 
##Nowcasting de casos hospitalizados
## N de dias do trim
inq.trim <- dados%>%
    filter(hospital ==1 & (pcr_sars2==1|classi_fin==5))%>%
    summarise(max(dt_sin_pri) - inq.data) %>% as.integer()
## Nowcasting
covid.casos.now <-  gera.nowcasting(dados, caso = TRUE, tipo = "covid",
                                    hospitalizados = TRUE, trim.now = inq.trim, window = 40)
## N de casos por datas de notificacao e primeiro sintoma
covid.ob.ncasos <- write.notificacoes.data(covid.ob.now$dados, tipo = "obitos_covid_data_sin", write.arq = FALSE)
covid.ncasos <- write.notificacoes.data(covid.casos.now$dados, tipo = "covid", write.arq = FALSE)
## junta nowcasting com n de notificacoes
covid.ob.merge <- prepara_dados2(covid.ob.now, covid.ob.ncasos)
covid.merge <- prepara_dados2(covid.casos.now, covid.ncasos)

################################################################################
## Calculo do IRF e do n de novas infeccoes por dia
################################################################################
## Calculo do IFR
## Separando data frame para facilitar
covid.ob.irf <- covid.ob.merge$now.pred.zoo
## IFR
IFR <- as.numeric(covid.ob.irf$estimate.merged.c[time(covid.ob.irf)==inq.data] / inq.IR)
## calculo do n de pessoas no compartimento IR e I a cada tempo
covid.ob.irf$IR  <- covid.ob.irf$estimate.merged.c / IFR
covid.ob.irf$I <- c(covid.ob.irf$IR[1], diff(covid.ob.irf$IR))
################################################################################
## Calculo do IHR e do n de novas infeccoes por dia
################################################################################
## Calculo do IHR
## Separando data frame para facilitar
covid.casos.irf <- covid.merge$now.pred.zoo
## IHR
IHR <- as.numeric(covid.casos.irf$estimate.merged.c[time(covid.casos.irf)==inq.data] / inq.IR)
## calculo do n de pessoas no compartimento IR e I a cada tempo
covid.casos.irf$IR  <- covid.casos.irf$estimate.merged.c / IHR
covid.casos.irf$I <- c(covid.casos.irf$IR[1], diff(covid.casos.irf$IR))


## Grafico de N de novas infecções por dia, estimados pelo IFF e pelo IHR
ggplot(covid.ob.irf, aes(Index, I)) +
    geom_line(aes(color="IFR")) +
    geom_line(data = covid.casos.irf, aes(Index, I, color="IHR"))
    ylab("Novas infecções")

Campinas  <-  list(Npop=Npop, inq.data=inq.data, inq.IR=inq.IR, dados=dados,
                covid.ob.now = covid.ob.now, covid.casos.now = covid.casos.now,
                covid.ob.ncasos = covid.ob.ncasos, covid.ncasos = covid.ncasos,
                covid.ob.merge = covid.ob.merge , covid.merge = covid.merge,
                covid.ob.irf = covid.ob.irf,
                IFR = IFR, covid.casos.irf = covid.casos.irf,
                IHR = IHR)
