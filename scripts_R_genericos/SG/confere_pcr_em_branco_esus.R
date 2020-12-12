## Conferencia de resultados de testes RT-PCR de residentes no municp
source("functions.R")
library(ggplot2)
library(devtools)
library(aweek)
library(knitr)
devtools::load_all("../../now_fcts/")
source('../../esus_analises/get.last.esus.R')

## Tabelas auxiliares (dicionarios)
siglas.estados <-
    data.frame(estado = c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17),
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
esus <- get.latest.esus.sqlite(fname="../../esus_analises/arquivos_esus.csv", dir="../../esus_analises/data_raw/")
my.db <- dbConnect(SQLite(), esus)

################################################################################
## Todos os testes
################################################################################
## N total de notificaces por data, estado, tipo do teste e estado do teste (nao só RT-PCR)
testes.estados.data <-
    tbl(my.db, "esus") %>%
    select(municipioIBGE, estadoIBGE, dataIni, estadoTeste, tipoTeste, resultadoTeste, classificacaoFinal) %>%
    mutate(estado = ifelse(!is.na(municipioIBGE)&is.na(estadoIBGE), substr(municipioIBGE,1,2), estadoIBGE)) %>%
    group_by(dataIni, estado, estadoTeste, tipoTeste, classificacaoFinal, resultadoTeste) %>%
    summarise(N=n()) %>%
    data.frame()
## Muda data para formato Date
testes.estados.data %<>%
    mutate(dataIni = as.Date(dataIni))

## Resumos semanais ##
## Tipo de testes por semana
tipo.testes.estados.sem  <-
    testes.estados.data %>%
    filter(dataIni > as.Date("2020-01-01") &
           dataIni <= Sys.Date()) %>%
    mutate(sem_sin = date2week(dataIni, numeric = TRUE)) %>%
    left_join(siglas.estados, by = "estado") %>%
    left_join(tipos.testes, by="tipoTeste") %>%
    group_by(sigla, sem_sin, teste) %>%
    summarise(N.casos = sum(N))
## Classificacao final por semana
classi.estados.sem  <-
    testes.estados.data %>%
    filter(dataIni > as.Date("2020-01-01") &
           dataIni <= Sys.Date()) %>%
    mutate(sem_sin = date2week(dataIni, numeric = TRUE),
           classif = ifelse(grepl("Confirm", classificacaoFinal), "Confirmado",
                            ifelse(grepl("Gripal", classificacaoFinal), "SG não especif.", classificacaoFinal))) %>%
    left_join(siglas.estados, by = "estado") %>%
    group_by(sigla, sem_sin, classif) %>%
    summarise(N.casos = sum(N))

## Situacao dos testes por semana
situacao.estados.sem  <-
    testes.estados.data %>%
    filter(dataIni > as.Date("2020-01-01") &
           dataIni <= Sys.Date()) %>%
    mutate(sem_sin = date2week(dataIni, numeric = TRUE),
           estadoTeste = factor(estadoTeste,
                                levels = c("Concluído", "Coletado", "Solicitado", "Exame Não Solicitado", NA))) %>%
    left_join(siglas.estados, by = "estado") %>%
    group_by(sigla, sem_sin, estadoTeste) %>%
    summarise(N.casos = sum(N))

## Resultado dos testes por semana
result.estados.sem  <-
    testes.estados.data %>%
    filter(dataIni > as.Date("2020-01-01") &
           dataIni <= Sys.Date()) %>%
    mutate(sem_sin = date2week(dataIni, numeric = TRUE),
           resultadoTeste = ifelse(is.na(resultadoTeste), "Não informado", resultadoTeste),
           resultadoTeste = factor(resultadoTeste,
                                   levels = c("Negativo",
                                              "Inconclusivo ou Indeterminado",
                                              "Não informado",
                                              "Positivo"))) %>%
    left_join(siglas.estados, by = "estado") %>%
    group_by(sigla, sem_sin, resultadoTeste) %>%
    summarise(N.casos = sum(N))

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
pcr.estados.data  <-
    tbl(my.db, "esus") %>%
    select(dataIni, estadoIBGE, municipioIBGE,
           estadoTeste, tipoTeste, resultadoTeste) %>%
    filter(
        tipoTeste == "RT-PCR" &
        estadoTeste!="Exame Não Solicitado" &
        !is.na(estadoTeste) &
        !is.na(dataIni)) %>%
    mutate(estado = ifelse(!is.na(municipioIBGE)&is.na(estadoIBGE), substr(municipioIBGE,1,2), estadoIBGE)) %>%
    group_by(estado, dataIni, resultadoTeste) %>%
    summarise(N = n()) %>%
    data.frame() %>%
    mutate(dataIni = as.Date(dataIni)) %>%
    filter(dataIni > as.Date("2020-01-01") &
           dataIni <= Sys.Date()) %>%
    mutate(sem_sin = date2week(dataIni, numeric = TRUE),
           resultadoTeste = ifelse(resultadoTeste=="null"|is.na(resultadoTeste), "Em branco (null + NA)", resultadoTeste)) 
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
                                   levels = c("Inconclusivo ou Indeterminado",
                                              "Negativo", "Positivo", "Em branco (null + NA)"))) %>%
    ggplot(aes(sem_sin, N.casos))+
    geom_area(aes(fill=resultadoTeste)) +
    theme_bw() +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
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
    xlab("Semana epidemiológica de sintomas") +
    ylab("Positividade RT-PCR") +
    facet_wrap(~sigla) +
    ggtitle("E-SUS VE, RT-PCR concluídos")

## % de testes PCR em branco por semana e estado
p3 <-
    pb.estados.sem %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(sem_sin)) +
    geom_line(aes(y=em.branco)) +
    theme_bw() +
    xlab("Semana epidemiológica de sintomas") +
    ylab("Proporção RT-PCR sem resultado") +
    facet_wrap(~sigla) +
    ggtitle("E-SUS VE, testes RT-PCR solicitados, coletados ou concluídos")

## Total de notificacoes por semana por tipo de teste e por estado
p4  <-
    tipo.testes.estados.sem %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(sem_sin, N.casos)) +
    geom_area(aes(fill = teste)) +
    theme_bw() +
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
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    theme(legend.position = c(0.85, 0.05)) +
    labs(fill = "Resultado do teste") +
    facet_wrap(~sigla, scales = "free") +
    ggtitle("E-SUS VE, todos os testes")

## Total de notificacoes por semana por situação do teste e por estado
p7  <-
    situacao.estados.sem %>%
    filter(!is.na(sigla)) %>%
    ggplot(aes(sem_sin, N.casos)) +
    geom_area(aes(fill = estadoTeste)) +
    theme_bw() +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    theme(legend.position = c(0.85, 0.05)) +
    labs(fill = "Situação do teste") +
    facet_wrap(~sigla, scales = "free") +
    ggtitle("E-SUS VE, todos os testes")

png("output/esus_testes_%1d.png", width = 900)
p4
p5
p6
p7
dev.off()

png("output/esus_rtpcr_%1d.png", width = 900)
p1
p2
p3
dev.off()

