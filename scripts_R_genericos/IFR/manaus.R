library(dplyr)
library(plyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(readr)
library(lubridate)
library(NobBS)
library(aweek)
source("../../nowcasting/fct/gera.nowcasting.R")
source("../../nowcasting/fct/write.notificacoes.data.R")
source("../../nowcasting/fct/prepara_dados2.R")
source("../../nowcasting/fct/get.last.date.R")
source("../../nowcasting/fct/read.sivep.R")
source("../../nowcasting/fct/preenche.now.R")


## Estimativas de total de infectados a partir do dados do inquérito de Manaus em
## Buss, Lewis F., et al. "Three-quarters attack rate of SARS-CoV-2 in the Brazilian Amazon during a largely unmitigated epidemic." Science 371.6526 (2021): 288-292.

################################################################################
## Primeira etapa: preparacao dos dados: nowcasting acumulado de casos e obitos,
## por data de primeiro sintoma
################################################################################
## Leitura dos dados mais recentes: sivep residentes, maiores de 15 anos
data.dir <- "../../dados/SIVEP-Gripe/"
raw.data <- read.sivep(dir = data.dir, escala = "municipio",
                       geocode = 1302603, data = get.last.date(data.dir))
## Filtra apenas casos de pessaso com mais de 15 anos
## Que é o universo amostral do inquérito
dados <- filter(as.integer(nu_idade_n) > 15)

## Nowcasting
## COVID ##
## trim de 7 dias
inq.trim <- 7
## Nowcasting de obitos, com data de sintoma como onset
covid.ob.now <-  gera.nowcasting(dados, caso = FALSE, tipo = "covid",
                               hospitalizados = FALSE, trim.now = inq.trim, window = 40,
                               obito_sin_pri = TRUE) 
##Nowcasting de casos hospitalizados
covid.casos.now <-  gera.nowcasting(dados, caso = TRUE, tipo = "covid",
                                    hospitalizados = TRUE, trim.now = inq.trim, window = 40)
## N de casos por datas de notificacao e primeiro sintoma
covid.ob.ncasos <- write.notificacoes.data(covid.ob.now$dados, tipo = "obitos_covid_data_sin", write.arq = FALSE)
covid.ncasos <- write.notificacoes.data(covid.casos.now$dados, tipo = "covid", write.arq = FALSE)
## junta nowcasting com n de casos por data de sintoma
covid.ob.merge <- prepara_dados2(covid.ob.now, covid.ob.ncasos)
covid.merge <- prepara_dados2(covid.casos.now, covid.ncasos)


################################################################################
## Dados do inquerito 
################################################################################
## A partir daqui dá para converter em uma funcao bem facilmente

## Projecoes populacionais usadas no artigo : https://demografiaufrn.net/laboratorios/lepp/
## Populacao total do municipio
PopTot <- 2216197
## Populacao maior que 15 anos em 2020
Npop <- 1675488

## Inquerito sorologico Buss et al 2020
## Material suplementar Table S2:
## https://science.sciencemag.org/content/sci/suppl/2020/12/07/science.abe9728.DC1/abe9728_Buss_SM.pdf
## Há prevalencias estimadas de várias maneiras para vários meses
## Usei a prevalência ajustada para sororeveresao em outubro (Seroreversion adjusted prevalence)
## Vale usar a prevalência corrigida para sexo e distr etária, e para sensibilidade e especificidade
## (Sensitivity and specificity adjusted prevalence (and age and sex-weighted)
## de meses anteriores para avaliar o comportamento

## Escreva aqui a Data da amostra 
inq.data <- as.Date("2020-10-17") - 7 # uso como referencia a data do fim da amostra menos 7 dias (tempo de resposta imune)
## Escreva aqui a prevalencia nesta data
preval <- 0.76
## Total de infectados e recuperados
inq.IR <- Npop * preval



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
covid.casos.ihr <- merge.zoo(covid.casos.ihr, n.not= zoo(covid.ncasos$n.not[,2],covid.ncasos$n.not[,1])) 
## Total de casos e de notificacoes semana epidemiologica
covid.casos.ihr.sem  <-
    covid.casos.ihr %>%
    as.data.frame() %>%
    mutate(semana = date2week(as.Date(row.names(.)), factor=TRUE, numeric=TRUE)) %>%
    dplyr::group_by(semana) %>%
    dplyr::summarise(n.casos = sum(I, na.rm=TRUE), not = sum(n.not, na.rm=TRUE), p.not =  not/n.casos) %>%
    gather(key = tipo, value = N, n.casos:not) 
## Prevalencias nas datas mais recentes na sivep(veja tb os graficos, abaixo)
## Usando IFR: duas vezes a populacao de referência
covid.ob.irf$IR[covid.ob.irf$IR==max(covid.ob.irf$IR, na.rm=TRUE)] / Npop
## Usando o IHR: 20% a mais que a populacao de referência
covid.casos.ihr$IR[covid.casos.ihr$IR==max(covid.casos.ihr$IR, na.rm=TRUE)] / Npop

## Guarda todos os resultados em uma lista
Manaus  <-  list(Npop=Npop, inq.data=inq.data, inq.IR=inq.IR, dados=dados,
                covid.ob.now = covid.ob.now, covid.casos.now = covid.casos.now,
                covid.ob.ncasos = covid.ob.ncasos, covid.ncasos = covid.ncasos,
                covid.ob.merge = covid.ob.merge , covid.merge = covid.merge,
                covid.ob.irf = covid.ob.irf,
                IFR = IFR, covid.casos.ihf = covid.casos.ihf,
                IHR = IHR)


################################################################################
## Graficos
################################################################################
## Grafico de N de novas infecções por dia, estimados pelo IFF e pelo IHR
covid.ob.irf %>%
    fortify() %>%
    ggplot(aes(Index, I)) +
    geom_line(aes(color="IFR")) +
    geom_line(data = fortify(covid.casos.ihr), aes(Index, I, color="IHR")) +
    geom_line(data = covid.casos.ihr, aes(Index, n.not, color="Notificados")) +
    ylab("Novas infecções") 
## Grafico de infectados + resistentes, estimador pelo IFF e IHR
covid.ob.irf %>%
    fortify() %>%
    ggplot(aes(Index, IR)) +
    geom_line(aes(color="IFR")) +
    geom_line(data = fortify(covid.casos.ihr), aes(Index, IR, color="IHR")) +
    geom_abline(intercept = Npop, col = "black") +
    ylab("Infectados + Resistentes") +
    theme_bw()
## Casos e notificacoes por semana epidemiologica
ggplot(covid.casos.ihr.sem, aes(semana, group = tipo)) +
    geom_col(aes(y=N,  fill= tipo), position = "stack")    
