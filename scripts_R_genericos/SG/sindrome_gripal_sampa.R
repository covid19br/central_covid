library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(zip)
library(aweek)
library(NobBS)
library(rmarkdown)
library(zoo)
source("../../nowcasting/fct/get.last.date.R")
set_week_start("Sunday")

## Leitura do arquivo importado do tabwin da PMSP
## http://tabnet.saude.prefeitura.sp.gov.br/cgi/deftohtm3.exe?secretarias/saude/TABNET/RCOVID19/covid19.def
## linha: Sem Epi Inicio Sintoma, Coluna: Dia Mês da Notif, Cnoteúdo: N de casos
## opção Colunas separadas por ";"
## Data de atualizacao dos dados, ver no site
data.atualiz <-  as.Date("2020-11-10")

data.dir <- "../../dados/municipio_SP/tabwin/SG/"
sgsp.raw <- read.csv2(paste0(data.dir,"sg_", get.last.date(data.dir),".csv"), na.strings="-")
sgsp.raw[is.na(sgsp.raw)] <- 0

## Cria data frame em formato tidy com datas finasis das semanas de sintomas e notificacao
## e N de casos
sgsp <- sgsp.raw[sgsp.raw$Sem.Epid.Início.Sintoma!="Total",-ncol(sgsp.raw)]
sgsp %<>%
    pivot_longer(cols = -Sem.Epid.Início.Sintoma, names_to="dt_not_X", values_to = "N.casos") %>%
    filter(N.casos > 0) %>%
    mutate(sem_sin = as.integer(substr(Sem.Epid.Início.Sintoma, 5, 6)),
           dt_not = as.Date(paste0(dt_not_X,".2020"), format = "X%d.%m.%Y"),
           sem_not = date2week(dt_not, numeric = TRUE),
           dt_sem_not = week2date(date2week(dt_not, floor_day = TRUE))+6,
           dt_sem_sin = week2date(paste0("2020-W",substr(Sem.Epid.Início.Sintoma, 5, 6),"-7"))) %>%
    filter(dt_sem_not <= data.atual & dt_sem_sin <= data.atual)
## Data.frame para o NobBS: cada linha o par de datas de sintoma e notificacao
sgsp2 <- sgsp[rep(1:nrow(sgsp), sgsp$N.casos), c("dt_sem_sin", "dt_sem_not")] %>% as.data.frame()

## nowcasting
sgsp.now <- NobBS(data = sgsp2,
                  now = max(sgsp2$dt_sem_sin),
                  units = "1 week",
                  onset_date = "dt_sem_sin",
                  report_date = "dt_sem_not",
                  specs=list(dist = "NB")
                  )
##png("SG_SP_2020_11_10%1d.png", width = 600)
## Grafico
sgsp %>%
    group_by(dt_sem_sin) %>%
    summarise(N.casos = sum(N.casos)) %>%
    merge(sgsp.now$estimates, by.x = "dt_sem_sin", by.y = "onset_date", all=TRUE) %>%
    ggplot(aes(dt_sem_sin, N.casos)) +
    geom_line( col = "blue") +
    geom_line( aes(y=estimate), col = "red") +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.25) +
    theme_bw() +
    xlab("Data final da semana epidemiológica de sintomas") +
    ylab("n de casos novos") +
    ggtitle("Síndrome Gripal no Mun de SP")

################################################################################
## Positividade ##
################################################################################
## Nº de Casos por Resultado do Teste segundo Sem Epid Início Sintoma
## Ano da Notificação: 2020
## Tipo de Teste: RT-PCR
## Situação do Teste: Solicitado, Coletado, Concluído
## Período: 2020

data.dir <- "../../dados/municipio_SP/tabwin/SG/"
pcrsp.raw <- read.csv2(paste0(data.dir,"rt_pcr_", get.last.date(data.dir),".csv"), na.strings="-")
pcrsp.raw[is.na(pcrsp.raw)] <- 0
pcrsp <- pcrsp.raw[pcrsp.raw$Sem.Epid.Início.Sintoma!="Total",]
pcrsp %<>%
    mutate(sem_sin = as.integer(substr(Sem.Epid.Início.Sintoma, 5, 6)),
           dt_sem_sin = week2date(paste0("2020-W",substr(Sem.Epid.Início.Sintoma, 5, 6),"-7")),
           positividade = 100*Positivo/(Positivo+ Negativo + Inconclusivo)) %>%
    filter(dt_sem_sin <= data.atual)

pcrsp2  <-
    select(pcrsp, -c(positividade,Total) ) %>%
    pivot_longer(cols= Negativo:Em.Branco, names_to="resultado", values_to = "N.casos")

## Graficos

## N de exames e positividade
positiv.zoo <-  zoo(pcrsp[, c("positividade", "Total")], order.by = pcrsp$dt_sem_sin)
plot(positiv.zoo, main = "Total de testes RT-PCR e positividade por semana")

## Stacked area por resultado
pcrsp2 %>%
    ggplot(aes(dt_sem_sin, N.casos))+
    geom_area(aes(fill=resultado)) +
    theme_bw() +
    xlab("Data final da semana epidemiológica de sintomas") +
    ylab("N de testes") +
    ggtitle("Testes RT-PCR de SG no Munic. de SP") +
    theme(legend.position = c(0.1, 0.8))
## dev.off()    
