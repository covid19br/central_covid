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

################################################################################
## Dados populacionais e do inquerito
################################################################################
## Populacao estimada em 2020 por faixa etaria SEADE
etaria <- read.csv2("municipio_sp_pop_sp_faixa_etaria_2020.csv")
## Populacao maior que 17 anos
Npop <- etaria[4,2]*2/5 + sum(etaria[5:16,2])
## Inquerito sorologico do Fleury: prevalencia, n de infectados estimado e data final do inquerito
## https://0dea032c-2432-4690-b1e5-636d3cbeb2bf.filesusr.com/ugd/6b3408_08bbcd940e9e4b84a29a7e64fce02464.pdf
inq.data <- as.Date("2020-06-26")
inq.IR <- Npop * 0.114

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
    geom_line() +
    geom_line(data=covid.casos.irf, aes(Index, I), col="blue")
