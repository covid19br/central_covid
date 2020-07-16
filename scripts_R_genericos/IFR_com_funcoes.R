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
## Dados populacionais e do inquerito
################################################################################
## Populacao estimada em 2020 por faixa etaria SEADE
etaria <- read.csv2("municipio_sp_pop_sp_faixa_etaria_2020.csv")
## Populacao maior que 17 anos
Npop <- etaria[4,2]*2/5 + sum(etaria[5:16,2])
## Inquerito sorologico do Fleury: prevalencia, n de infectados estimado e data final do inquerito
## https://0dea032c-2432-4690-b1e5-636d3cbeb2bf.filesusr.com/ugd/6b3408_08bbcd940e9e4b84a29a7e64fce02464.pdf
inq.data <- as.Date("2020-06-24") - 7 #data do fim do estduo menos 7 dias da janela)
inq.IR <- Npop * 0.114

################################################################################
## dados n de notificacoes SEADE
################################################################################
seade <- read.csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv") %>%
    filter(codigo_ibge == 3550308)

################################################################################
## Primeira etapa: preparacao dos dados: nowcasting acumulado de casos e obitos,
## por data de primeiro sintoma
################################################################################
## Leitura dos dados: sivep residentes São Paulo, maiores de 17 anos
data.dir <- "../dados/estado_SP/SRAG_hospitalizados/dados/"
dados <- read.sivep(dir = data.dir, escala = "municipio",
                    geocode = 3550308, data = get.last.date(data.dir)) %>%
    filter(as.integer(nu_idade_n) > 17)
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
## trim de 7 dias
inq.trim <- 7
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
covid.casos.ihr <- covid.merge$now.pred.zoo
## IHR
IHR <- as.numeric(covid.casos.ihr$estimate.merged.c[time(covid.casos.ihr)==inq.data] / inq.IR)
## calculo do n de pessoas no compartimento IR e I a cada tempo
covid.casos.ihr$IR  <- covid.casos.ihr$estimate.merged.c / IHR
covid.casos.ihr$I <- c(covid.casos.ihr$IR[1], diff(covid.casos.ihr$IR))
## Junta Casos notificados
covid.casos.ihr <- merge.zoo(covid.casos.ihr, notificados=zoo(seade$casos_novos, as.Date(seade$datahora)))
covid.casos.ihr$semana <- date2week(time(covid.casos.ihr))
## Total de casos e de notificacoes semana epidemiologica
covid.casos.ihr.sem  <-
    covid.casos.ihr %>%
    as.data.frame() %>%
    mutate(semana = date2week(as.Date(row.names(.)), factor=TRUE, numeric=TRUE)) %>%
    dplyr::group_by(semana) %>%
    dplyr::summarise(n.casos = sum(I, na.rm=TRUE), not = sum(notificados, na.rm=TRUE), p.not =  not/n.casos) %>%
    gather(key = tipo, value = N, n.casos:not) %>%
    filter(semana < 28)
    

################################################################################
## Graficos
################################################################################
## Grafico de N de novas infecções por dia, estimados pelo IFF e pelo IHR
ggplot(covid.ob.irf, aes(Index, I)) +
    geom_line(aes(color="IFR")) +
    geom_line(data = covid.casos.ihr, aes(Index, I, color="IHR")) +
    geom_line(data = covid.casos.ihr, aes(Index, notificados, color="Notificados")) +
    ylab("Novas infecções")
## Casos e notificacoes por semana epidemiologica
ggplot(covid.casos.ihr.sem, aes(semana, group = tipo)) +
    geom_col(aes(y=N,  fill= tipo), position = "stack")    

Sampa  <-  list(Npop=Npop, inq.data=inq.data, inq.IR=inq.IR, dados=dados,
                covid.ob.now = covid.ob.now, covid.casos.now = covid.casos.now,
                covid.ob.ncasos = covid.ob.ncasos, covid.ncasos = covid.ncasos,
                covid.ob.merge = covid.ob.merge , covid.merge = covid.merge,
                covid.ob.irf = covid.ob.irf,
                IFR = IFR, covid.casos.ihf = covid.casos.ihf,
                IHR = IHR)
