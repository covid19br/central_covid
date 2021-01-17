## Conferencia de resultados de testes RT-PCR de residentes no municipio
library(plyr)
library(magrittr)
source("functions.R")
library(tidyr)
library(ggplot2)
library(devtools)
library(aweek)
library(knitr)

## devtools::load_all("../../now_fcts/")
## source('../../esus_analises/get.last.esus.R')

## Tabelas auxiliares (dicionarios)
siglas.estados <-
    data.frame(estado = as.character( c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17)),
               sigla = c('AC','AL','AM','AP','BA','CE','DF','ES',
                         'GO','MA','MG','MS','MT','PA','PB','PE',
                         'PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')) 

tipos.testes <- data.frame(
    tipoTeste = c(
        "RT-PCR",
        "TESTE RÁPIDO - ANTICORPO",
        NA,
        "TESTE RÁPIDO - ANTÍGENO",
        "Imunoensaio por Eletroquimioluminescência - ECLIA IgG",
        "Imunoensaio por Eletroquimioluminescência – ECLIA",
        "Enzimaimunoensaio - ELISA IgM",
        "Quimioluminescência - CLIA",
        "Enzimaimunoensaio – ELISA",
        "Teste rápido"),
    teste = c("RT-PCR",
              NA,
              "Testes rápidos",
              "Testes rápidos",
              "Imunoensaio",
              "Imunoensaio",
              "Imunoensaio",
              "Imunoensaio",
              "Imunoensaio",
              "Testes rápidos")
    )

################################################################################
## Leitura e sumarização dos dados
################################################################################
## Conecta em uma base de dados da ultima versão da E-SUS disponivel
## bases compartilhadas por Osawldo Cruz, PROCC - FioCruz
## esus <- get.latest.esus.sqlite(fname="../../esus_analises/arquivos_esus.csv", dir="../../esus_analises/data_raw/")
esus <- "../../esus_analises/data_raw/esus_brasil"
my.db <- dbConnect(SQLite(), esus)
tabelas <- dbListTables(my.db)
################################################################################
## Todos os testes
################################################################################
## N total de notificaces por data, estado, tipo do teste e estado do teste (nao só RT-PCR)
## Passa por todas as tabelas de estado, paralelizado
cl <- makePSOCKcluster(detectCores()-2)
clusterExport(cl, "esus")
clusterEvalQ(cl,{
    library(RSQLite)
    library(magrittr)
    library(dplyr)
    my.db <- dbConnect(SQLite(), esus)
}
)
testes.estados.data <- parLapply(cl, tabelas, sumario.1)
stopCluster(cl)
## Converte para dataframe e muda data de inicio para formato data
testes.estados.data %<>%
    ldply(data.frame) %>%
    mutate(dataInicioSintomas = as.Date(dataInicioSintomas))

## Resumos semanais ##
## Tipo de testes por semana
tipo.testes.estados.sem  <-
    testes.estados.data %>%
    filter(dataInicioSintomas > as.Date("2020-01-01") &
           dataInicioSintomas <= Sys.Date()) %>%
    mutate(sem_sin = week2date(date2week(dataInicioSintomas, floor_day=TRUE))) %>%
    left_join(siglas.estados, by = "estado") %>%
    left_join(tipos.testes, by="tipoTeste") %>%
    group_by(sigla, sem_sin, teste) %>%
    summarise(N.casos = sum(N))
## Classificacao final por semana
classi.estados.sem  <-
    testes.estados.data %>%
    filter(dataInicioSintomas > as.Date("2020-01-01") &
           dataInicioSintomas <= Sys.Date()) %>%
    mutate(sem_sin = week2date(date2week(dataInicioSintomas, floor_day=TRUE)),
           classif = ifelse(grepl("Confirm", classificacaoFinal), "Confirmado",
                            ifelse(grepl("Gripal", classificacaoFinal), "SG não especif.", classificacaoFinal))) %>% 
    left_join(siglas.estados, by = "estado") %>%
    group_by(sigla, sem_sin, classif) %>%
    summarise(N.casos = sum(N))

## Situacao dos testes por semana
situacao.estados.sem  <-
    testes.estados.data %>%
    filter(dataInicioSintomas > as.Date("2020-01-01") &
           dataInicioSintomas <= Sys.Date()) %>%
    mutate(sem_sin = week2date(date2week(dataInicioSintomas, floor_day=TRUE)),
           estadoTeste = factor(estadoTeste,
                                levels = c("Concluído", "Coletado", "Solicitado", "Exame Não Solicitado", NA))) %>%
    left_join(siglas.estados, by = "estado") %>%
    group_by(sigla, sem_sin, estadoTeste) %>%
    summarise(N.casos = sum(N))

## Resultado dos testes por semana
result.estados.sem  <-
    testes.estados.data %>%
    filter(dataInicioSintomas > as.Date("2020-01-01") &
           dataInicioSintomas <= Sys.Date() &
           estadoTeste!="Exame Não Solicitado"&
           !is.na(estadoTeste)) %>%
    mutate(sem_sin = week2date(date2week(dataInicioSintomas, floor_day=TRUE)),
           resultadoTeste = ifelse(is.na(resultadoTeste), "Não informado", resultadoTeste),
           resultadoTeste = factor(resultadoTeste,
                                   levels = c("Não informado",
                                              "Inconclusivo ou Indeterminado",
                                              "Negativo",
                                              "Positivo"))) %>%
    left_join(siglas.estados, by = "estado") %>%
    group_by(sigla, sem_sin, resultadoTeste) %>%
    summarise(N.casos = sum(N))

################################################################################
## Testes rapidos
################################################################################
## Resultados testes rapidos por semana
result.rap.estados.sem  <-
    testes.estados.data %>%
    left_join(tipos.testes, by="tipoTeste") %>%
    filter(dataInicioSintomas > as.Date("2020-01-01") &
           dataInicioSintomas <= Sys.Date() &
           teste == "Testes rápidos" &
           estadoTeste!="Exame Não Solicitado"&
           !is.na(estadoTeste)) %>%
    mutate(sem_sin = week2date(date2week(dataInicioSintomas, floor_day=TRUE)),
           resultadoTeste = ifelse(is.na(resultadoTeste), "Não informado", resultadoTeste),
           resultadoTeste = factor(resultadoTeste,
                                   levels = c("Não informado",
                                              "Inconclusivo ou Indeterminado",
                                              "Negativo",
                                              "Positivo"))) %>%
    left_join(siglas.estados, by = "estado") %>%
    group_by(sigla, sem_sin, resultadoTeste) %>%
    summarise(N.casos = sum(N))

## Positividade de testes rápidos
positividade.rapidos.sem <-
    testes.estados.data %>%
    left_join(tipos.testes, by="tipoTeste") %>%
    filter(dataInicioSintomas > as.Date("2020-01-01") &
           dataInicioSintomas <= Sys.Date() &
           teste == "Testes rápidos") %>%
    mutate(sem_sin = week2date(date2week(dataInicioSintomas, floor_day=TRUE)),
           resultadoTeste = ifelse(is.na(resultadoTeste), "Não informado", resultadoTeste)) %>%
    left_join(siglas.estados, by = "estado") %>%
    group_by(sigla, sem_sin, resultadoTeste) %>%
    summarise(N.casos = sum(N)) %>%
    pivot_wider(names_from = resultadoTeste, values_from = N.casos, values_fill = 0) %>%
    rename(Branco = "Não informado", Indet = "Inconclusivo ou Indeterminado") %>%
    group_by(sigla, sem_sin) %>%
    summarise(N.testes = Positivo + Negativo + Branco + Indet,
              N.resultados = N.testes - Branco,
              positiv = Positivo/N.resultados,
              em.branco = Branco/N.testes)

### Tabelas
testes.estados.data %>%
    mutate(classificacaoFinal = ifelse(grepl("Epidemiol", classificacaoFinal), "Confirmado Clínico-Epidemiológico",
                                ifelse(grepl("Gripal", classificacaoFinal), "SG Não Especificada",
                                ifelse(grepl("Labor", classificacaoFinal), "Confirmação Laboratorial", classificacaoFinal)))) %>%
    with(., xtabs(N ~ classificacaoFinal + resultadoTeste, addNA=TRUE)) %>% addmargins() %>% kable()

with(testes.estados.data, xtabs(N ~ tipoTeste + resultadoTeste, addNA=TRUE)) %>% addmargins() %>% kable()

################################################################################
## RT-PCR
################################################################################
## n de testes pcr por data, resultado e estado
cl <- makePSOCKcluster(detectCores()-2)
clusterExport(cl, "esus")
clusterEvalQ(cl,{
    library(RSQLite)
    library(magrittr)
    library(dplyr)
    library(aweek)
    my.db <- dbConnect(SQLite(), esus)
}
)
pcr.estados.data <- parLapply(cl, tabelas, sumario.2)
stopCluster(cl)
## Converte para dataframe e muda data de inicio para formato data
pcr.estados.data %<>%
    ldply(data.frame) %>%
    mutate(dataInicioSintomas = as.Date(dataInicioSintomas))


## Por semana
pcr.estados.sem  <- 
    pcr.estados.data %>%
    left_join(siglas.estados, by = "estado") %>%
    group_by(sigla, sem_sin, resultadoTeste) %>%
    summarise(N.casos = sum(N))

##Positividade e em branco por estado
pb.estados.sem  <- 
    pcr.estados.sem %>%
    pivot_wider(names_from = resultadoTeste, values_from=N.casos, values_fill=0) %>%
    rename(Branco = "Em branco (null + NA)", Indet = "Inconclusivo ou Indeterminado") %>%
    group_by(sigla, sem_sin) %>%
    summarise(N.testes = Positivo + Negativo + Branco + Indet,
              N.resultados = N.testes - Branco,
              positiv = Positivo/N.resultados,
              em.branco = Branco/N.testes)

################################################################################
## Plots
################################################################################
## N de testes PCR por resultado e semana, por estado
p1 <-
    pcr.estados.sem %>%
    filter(!is.na(sigla)) %>%
    mutate(resultadoTeste = factor(resultadoTeste,
                                   levels = c("Em branco (null + NA)", "Inconclusivo ou Indeterminado",
                                              "Negativo", "Positivo"))) %>%
    ggplot(aes(sem_sin, N.casos))+
    geom_area(aes(fill=resultadoTeste)) +
    theme_bw() +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "1 months",
                 date_labels = "%m/%y") +
    theme(legend.position = c(0.85, 0.05)) +
    labs(fill = "Resultado do RT-PCR") +
    facet_wrap(~sigla, scales = "free")+
    ggtitle("E-SUS VE, testes RT-PCR solicitados, coletados ou concluídos")

## Positividade de PCR por semana e estado
p2 <-
    pb.estados.sem %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(sem_sin)) +
    geom_line(aes(y=positiv)) +
    theme_bw() +
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "1 months",
                 date_labels = "%m/%y") +
    xlab("Semana epidemiológica de sintomas") +
    ylab("Positividade") +
    facet_wrap(~sigla)

## % de testes PCR em branco por semana e estado
p3 <-
    pb.estados.sem %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(sem_sin)) +
    geom_line(aes(y=em.branco)) +
    theme_bw() +
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "1 months",
                 date_labels = "%m/%y") +
    xlab("Semana epidemiológica de sintomas") +
    ylab("Proporção de testes sem resultado") +
    facet_wrap(~sigla)

## Total de notificacoes por semana por tipo de teste e por estado
p4  <-
    tipo.testes.estados.sem %>%
    filter(!is.na(sigla)&!is.na(teste)) %>%
    ggplot(aes(sem_sin, N.casos)) +
    geom_area(aes(fill = teste)) +
    theme_bw() +
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "1 months",
                 date_labels = "%m/%y") +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    theme(legend.position = c(0.85, 0.05)) +
    labs(fill = "Tipo de teste") +
    facet_wrap(~sigla, scales = "free") +
    ggtitle("E-SUS VE, todos os testes")

## Total de notificacoes por semana por classificacao e por estado
p5  <-
    classi.estados.sem %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(sem_sin, N.casos)) +
    geom_area(aes(fill = classif)) +
    theme_bw() +
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "1 months",
                 date_labels = "%m/%y") +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    theme(legend.position = c(0.85, 0.05)) +
    labs(fill = "Classificação final") +
    facet_wrap(~sigla, scales = "free")+
    ggtitle("E-SUS VE, todos os testes")

## Total de notificacoes por semana por resultado do teste e por estado
p6  <-
    result.estados.sem %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(sem_sin, N.casos)) +
    geom_area(aes(fill = resultadoTeste)) +
    theme_bw() +
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "1 months",
                 date_labels = "%m/%y") +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    theme(legend.position = c(0.85, 0.05)) +
    labs(fill = "Resultado do teste") +
    facet_wrap(~sigla, scales = "free") 

## Total de notificacoes por semana por situação do teste e por estado
p7  <-
    situacao.estados.sem %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(sem_sin, N.casos)) +
    geom_area(aes(fill = estadoTeste)) +
    theme_bw() +
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "1 months",
                 date_labels = "%m/%y") +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    theme(legend.position = c(0.85, 0.05)) +
    labs(fill = "Situação do teste") +
    facet_wrap(~sigla, scales = "free") +
    ggtitle("E-SUS VE, todos os testes")

## Trajetorias entre N de teste rapido e RT-PCR
## semana maxima: tira as ultimas duas semanas
sem.max <- max(c(positividade.rapidos.sem$sem_sin, pb.estados.sem$sem_sin)) - 14
## O grafico
p8 <-
    merge(positividade.rapidos.sem, pb.estados.sem, all=TRUE, by =c("sigla", "sem_sin"), suffixes=c(".rap",".pcr")) %>%
    filter(!is.na(sigla)&sem_sin <= sem.max) %>% 
    ggplot(aes(N.testes.rap, N.testes.pcr)) +
    geom_path(aes(colour=sem_sin)) +
    theme_bw() +
    ylab("N testes RT-PCR") +
    xlab("N testes rápidos") +
    facet_wrap(~sigla, scales = "free") +
    theme(legend.position = c(0.85, 0.05)) +
    labs(colour = "Semana do sintoma") +
    scale_color_viridis_c(trans= "date") +
    ggtitle("E-SUS VE, testes RT-PCR e Testes rápidos")
## Proporçao de de RT-PCR
p9 <-
    merge(positividade.rapidos.sem, pb.estados.sem, all=TRUE, by =c("sigla", "sem_sin"), suffixes=c(".rap",".pcr")) %>%
    mutate(prop.pcr = N.testes.pcr / (N.testes.pcr + N.testes.rap)) %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(sem_sin, prop.pcr)) +
    geom_line() +
    theme_bw() +
    scale_x_date(date_breaks = "3 months",
                 date_minor_breaks = "1 months",
                 date_labels = "%m/%y") +
    ylab("Proporção de testes RT-PCR") +
    xlab("Semana do primeiro sintoma") +
    facet_wrap(~sigla) +
    ggtitle("E-SUS VE, testes RT-PCR e Testes rápidos")

## Relação entre positividades
p10 <-
    merge(positividade.rapidos.sem, pb.estados.sem, all=TRUE, by =c("sigla", "sem_sin"), suffixes=c(".rap",".pcr")) %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(positiv.rap, positiv.pcr)) +
    geom_point() +
    theme_bw() +
    ylab("Positividade RT-PCR") +
    xlab("Positividade testes rápidos") +
    facet_wrap(~sigla) +
    ggtitle("E-SUS VE, testes RT-PCR e Testes rápidos")


## Salva os gráficos
png("output/esus_testes_%1d.png", width = 900)
p4
p5
p6 + ggtitle("E-SUS VE, todos os testes")
p7
dev.off()

png("output/esus_rapidos_%1d.png", width = 900)
p6 %+%
    filter(result.rap.estados.sem, !is.na(sigla)) +
    ggtitle("E-SUS VE, testes rápidos  concluídos")
p2 %+%
    filter(positividade.rapidos.sem, !is.na(sigla)) +
    ggtitle("E-SUS VE, testes rápidos  concluídos")
p3 %+%
    filter(positividade.rapidos.sem, !is.na(sigla)) +
    ggtitle("E-SUS VE, testes rápidos solicitados, coletados ou concluídos")
dev.off()



png("output/esus_rtpcr_%1d.png", width = 900)
p1
p2 + ggtitle("E-SUS VE, RT-PCR concluídos")
p3 + ggtitle("E-SUS VE, testes RT-PCR solicitados, coletados ou concluídos")
dev.off()

png("output/compara_testes_%1d.png", width = 900)
p8
p9
p10
dev.off()
