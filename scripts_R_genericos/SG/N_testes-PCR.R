## Conferencia de resultados de testes RT-PCR por estado
source("functions.R")
library(ggplot2)
library(devtools)
library(aweek)
library(knitr)
library(zoo)
devtools::load_all("../../now_fcts/")
source('../../esus_analises/get.last.esus.R')


## Tabelas auxiliares (dicionarios)
siglas.estados <-
    data.frame(estado = c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17),
               sigla = c('AC','AL','AM','AP','BA','CE','DF','ES',
                         'GO','MA','MG','MS','MT','PA','PB','PE',
                         'PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')) 

################################################################################
## Leitura e sumarização dos dados
################################################################################
## Conecta em uma base de dados da ultima versão da E-SUS disponivel
## bases compartilhadas por Oswaldo Cruz, PROCC - FioCruz
esus <- get.latest.esus.sqlite(fname="../../esus_analises/arquivos_esus.csv", dir="../../esus_analises/data_raw/")
my.db <- dbConnect(SQLite(), esus)

## N total de notificaces por data, estado, tipo do teste e estado do teste (nao só RT-PCR)
pcr.estados.mes <-
    tbl(my.db, "esusbr") %>%
    select(municipioIBGE, estadoIBGE, numeroCnes, dataTeste, estadoTeste, resultadoTeste, tipoTeste) %>%
    filter(tipoTeste == "RT-PCR" & !is.na(dataTeste)) %>%
    mutate(estado = ifelse(!is.na(municipioIBGE)&is.na(estadoIBGE), substr(municipioIBGE,1,2), estadoIBGE),
           mesTeste = substr(dataTeste, 6,7))  %>%
    group_by(mesTeste, estado, estadoTeste, resultadoTeste) %>%
    summarise(N=n()) %>%
    data.frame()

## Tabelas para SP
## Por estado do teste
pcr.estados.mes %>%
    left_join(siglas.estados, by = "estado") %>%
    filter(sigla=="SP") %>%
    with(., xtabs(N ~ mesTeste + estadoTeste, addNA = TRUE)) %>% addmargins() %>% kable()
## Por resultado do teste
pcr.estados.mes %>%
    left_join(siglas.estados, by = "estado") %>%
    filter(sigla=="SP"& estadoTeste!="Exame Não Solicitado") %>%
    with(., xtabs(N ~ mesTeste + resultadoTeste, addNA = TRUE)) %>% addmargins() %>% kable()

## Grafico
png("output/esus_pcr_mes.png", width = 900)
pcr.estados.mes %>%
    left_join(siglas.estados, by = "estado") %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(mesTeste, N)) +
    geom_col(aes(fill=resultadoTeste)) +
    theme_bw() +
    xlab("Mês da realização do teste") +
    ylab("N de testes") +
    theme(legend.position = c(0.85, 0.05)) +
    labs(fill = "Resultado do RT-PCR") +
    facet_wrap(~sigla, scales = "free")+
    ggtitle("E-SUS VE Semana 50, testes RT-PCR concluídos")  
dev.off()
