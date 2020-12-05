## Conferencia de resultados de testes RY-PCR de residentes no municp
library(dplyr)
library(ggplot2)
library(devtools)
library(aweek)
library(knitr)


devtools::load_all("../../../now_fcts/")
data.dir <- "../../eSUS-VE/" ## Tenho um clone do nosso repo de bases E-SUS em ../../

## Dados do municipio
estado.name = "são paulo"
mun.name = "São Paulo"
geocode_cidade <- get.geocode(nome_municipio = mun.name, sigla = "SP")


f1 <- function(x){
    read.esus.generica(x) %>%
        filter(municipioibge == geocode_cidade) %>%
        select(datainiciosintomas, datateste, datanotificacao, dataencerramento, 
               estadoteste, tipoteste, resultadoteste, classificacaofinal)
}

## Leitura do E-SUS para Sampa,  data de 05/6/2020
brasil.06.05 <- read.esus.generica(paste0(data.dir,"esus-ve_2020_06_05.csv.xz"))
sampa.06.05 <- filter(brasil.06.05, municipio == "São Paulo") %>%
    select(datainiciosintomas, datateste,
               estadoteste, datateste, tipoteste, resultadoteste)
    
save.image()

## Leitura do E-SUS do estado,  data de 28/11/2020
sp1 <- read.esus.generica(paste0(data.dir,"esus-ve_sp-1-2020_11_28.csv.bz2"))
sp2 <- read.esus.generica(paste0(data.dir,"esus-ve_sp-2-2020_11_28.csv.bz2"))
sp3 <- read.esus.generica(paste0(data.dir,"esus-ve_sp-3-2020_11_28.csv.bz2"))
sp4 <- read.esus.generica(paste0(data.dir,"esus-ve_sp-4-2020_11_28.csv.bz2"))
sp5 <- read.esus.generica(paste0(data.dir,"esus-ve_sp-5-2020_11_28.csv.bz2"))

sp.11.28 <-  rbind(sp1, sp2, sp3, sp4, sp5)

rm(sp1, sp2, sp3, sp4, sp5)

save.image()


## Leitura dados Sampa
sampa1 <- f1(paste0(data.dir,"esus-ve_sp-1-2020_11_28.csv.bz2"))
sampa2 <- f1(paste0(data.dir,"esus-ve_sp-2-2020_11_28.csv.bz2"))
sampa3 <- f1(paste0(data.dir,"esus-ve_sp-3-2020_11_28.csv.bz2"))
sampa4 <- f1(paste0(data.dir,"esus-ve_sp-4-2020_11_28.csv.bz2"))
sampa5 <- f1(paste0(data.dir,"esus-ve_sp-5-2020_11_28.csv.bz2"))

sampa.11.28 <-  rbind(sampa1, sampa2, sampa3, sampa4, sampa5)

rm(sampa1, sampa2, sampa3, sampa4, sampa5)

save.image()


## Leitura do E-SUS do estado,  ultima data disponivel
sampa1 <- read.esus.generica(paste0(data.dir,"esus-ve_sp-1-",ultima.data,".csv.bz2"))

ultima.data <- get.last.date(data.dir)
sampa1 <- f1(paste0(data.dir,"esus-ve_sp-1-",ultima.data,".csv.bz2")) %>% filter(municipioibge == geocode_cidade)
sampa2 <- f1(paste0(data.dir,"esus-ve_sp-2-",ultima.data,".csv.bz2")) %>% filter(municipioibge == geocode_cidade)
sampa3 <- f1(paste0(data.dir,"esus-ve_sp-3-",ultima.data,".csv.bz2")) %>% filter(municipioibge == geocode_cidade)
sampa4 <- f1(paste0(data.dir,"esus-ve_sp-4-",ultima.data,".csv.bz2")) %>% filter(municipioibge == geocode_cidade)
sampa5 <- f1(paste0(data.dir,"esus-ve_sp-5-",ultima.data,".csv.bz2")) %>% filter(municipioibge == geocode_cidade)

sampa.ultima <-  rbind(sampa1, sampa2, sampa3, sampa4, sampa5)

rm(sampa1, sampa2, sampa3, sampa4, sampa5)

save.image()

## Ultima semana 
## Tabela de Estado do teste x resultado, para RT-PCR
with(subset (sampa.ultima, tipoteste == "RT-PCR"),
     table(resultadoteste, estadoteste, useNA="always")) %>% addmargins() %>% kable()

## Por semanas
sampa.ultima.sem  <- 
    sampa.ultima %>%
    filter(tipoteste == "RT-PCR" & estadoteste!="Exame Não Solicitado" & !is.na(estadoteste) &
           !is.na(datainiciosintomas) & datainiciosintomas > as.Date("2020-01-01") &
           datainiciosintomas <= as.Date(ultima.data,"%Y_%m_%d")) %>%
    mutate(sem_sin = date2week(datainiciosintomas, numeric = TRUE),
           resultadoteste = ifelse(resultadoteste=="null"|is.na(resultadoteste), "Em branco (null + NA)", resultadoteste)) %>%
    group_by(sem_sin, resultadoteste) %>%
    summarise(N.casos = n())
## Grafico
png(paste0("RT-PCR_ESUS_SAMPA_",ultima.data,".png"), width = 900)
sampa.ultima.sem %>%
    fortify() %>%
    ggplot(aes(sem_sin, N.casos))+
    geom_area(aes(fill=resultadoteste)) +
    theme_bw() +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    ggtitle(paste0("Testes RT-PCR de SG no Munic. de SP, E-Sus de ",
                   format(as.Date(ultima.data,"%Y_%m_%d"), "%d/%m/%Y"))) +
    theme(legend.position = c(0.1, 0.8)) +
    labs(fill = "Resultado do RT-PCR")
dev.off()


## até 05/06/2020

## Tabela de Estado do teste x resultado, para RT-PCR
with(subset (sampa.06.05, tipoteste == "RT-PCR"),
     table(resultadoteste, estadoteste, useNA="always")) %>% addmargins() %>% kable()
## Tabela tipo de teste x estado do teste
table(sampa.06.05$tipoteste, sampa.06.05$estadoteste, useNA = "always") %>% addmargins() %>% kable()

## Tabela de resultados por tipo
table(sampa.06.05$tipoteste, sampa.06.05$resultadoteste, useNA = "always") %>% addmargins() %>% kable()


## Por semanas
sampa.06.05.sem  <- 
    sampa.06.05 %>%
    filter(tipoteste == "RT-PCR" & estadoteste!="Exame Não Solicitado" & !is.na(estadoteste) &
           !is.na(datainiciosintomas) & datainiciosintomas > as.Date("2020-01-01") &
           datainiciosintomas <= as.Date("2020-06-05")) %>%
    mutate(sem_sin = date2week(datainiciosintomas, numeric = TRUE),
           resultadoteste = ifelse(resultadoteste=="null"|is.na(resultadoteste), "Em branco (null + NA)", resultadoteste)) %>%
    group_by(sem_sin, resultadoteste) %>%
    summarise(N.casos = n())
## Grafico
png(paste0("RT-PCR_ESUS_SAMPA_2020_06_05.png"), width = 900)
sampa.06.05.sem %>%
    fortify() %>%
    ggplot(aes(sem_sin, N.casos))+
    geom_area(aes(fill=resultadoteste)) +
    theme_bw() +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    ggtitle("Testes RT-PCR de SG no Munic. de SP, E-Sus de 05/06/2020") +
    theme(legend.position = c(0.1, 0.8)) +
    labs(fill = "Resultado do RT-PCR")
dev.off()

################################################################################
## Campinas
################################################################################

## Dados do municipio
estado.name = "são paulo"
mun.name = "Campinas"
geocode_cidade <- get.geocode(nome_municipio = mun.name, sigla = "SP")

campinas.ultima <- filter(sp.11.28, municipioibge == geocode_cidade)
    
## Tabela de Estado do teste x resultado, para RT-PCR
with(subset (campinas.ultima, tipoteste == "RT-PCR"),
     table(resultadoteste, estadoteste, useNA="always")) %>% addmargins() %>% kable()

## Por semanas
campinas.ultima.sem  <- 
    campinas.ultima %>%
    filter(tipoteste == "RT-PCR" & estadoteste!="Exame Não Solicitado" & !is.na(estadoteste) &
           !is.na(datainiciosintomas) & datainiciosintomas > as.Date("2020-01-01") &
           datainiciosintomas <= as.Date(ultima.data,"%Y_%m_%d")) %>%
    mutate(sem_sin = factor(date2week(datainiciosintomas, numeric = TRUE)),
           resultadoteste = factor(ifelse(resultadoteste=="null"|is.na(resultadoteste),
                                          "Em branco (null + NA)", resultadoteste))) %>%
    group_by(sem_sin, resultadoteste, .drop=FALSE) %>%
    summarise(N.casos = n())
## Grafico
png(paste0("RT-PCR_ESUS_CAMPINAS_",ultima.data,".png"), width = 900)
campinas.ultima.sem %>%
    fortify() %>%
    ggplot(aes(as.integer(sem_sin), N.casos))+
    geom_area(aes(fill=resultadoteste)) +
    theme_bw() +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    ggtitle(paste0("Testes RT-PCR de SG no Munic. de Campinas, E-Sus de ",
                   format(as.Date(ultima.data,"%Y_%m_%d"), "%d/%m/%Y"))) +
    theme(legend.position = c(0.1, 0.8)) +
    labs(fill = "Resultado do RT-PCR")
dev.off()


################################################################################
## Diadema
################################################################################

## Dados do municipio
estado.name = "são paulo"
mun.name = "Diadema"
geocode_cidade <- get.geocode(nome_municipio = mun.name, sigla = "SP")

diadema.ultima <- filter(sp.11.28, municipioibge == geocode_cidade)
    
## Tabela de Estado do teste x resultado, para RT-PCR
with(subset (diadema.ultima, tipoteste == "RT-PCR"),
     table(resultadoteste, estadoteste, useNA="always")) %>% addmargins() %>% kable()

## Por semanas
diadema.ultima.sem  <- 
    diadema.ultima %>%
    filter(tipoteste == "RT-PCR" & estadoteste!="Exame Não Solicitado" & !is.na(estadoteste) &
           !is.na(datainiciosintomas) & datainiciosintomas >= as.Date("2020-01-01") &
           datainiciosintomas <= as.Date(ultima.data,"%Y_%m_%d")) %>%
    mutate(sem_sin = factor(date2week(datainiciosintomas, numeric = TRUE)),
           resultadoteste = factor(ifelse(resultadoteste=="null"|is.na(resultadoteste),
                                          "Em branco (null + NA)", resultadoteste))) %>%
    group_by(sem_sin, resultadoteste, .drop=FALSE) %>%
    summarise(N.casos = n())
## Grafico
png(paste0("RT-PCR_ESUS_DIADEMA_",ultima.data,".png"), width = 900)
diadema.ultima.sem %>%
    fortify() %>%
    ggplot(aes(as.integer(sem_sin), N.casos))+
    geom_area(aes(fill=resultadoteste)) +
    theme_bw() +
    xlab("Semana epidemiológica de sintomas") +
    ylab("N de testes") +
    ggtitle(paste0("Testes RT-PCR de SG no Munic. de Diadema, E-Sus de ",
                   format(as.Date(ultima.data,"%Y_%m_%d"), "%d/%m/%Y"))) +
    theme(legend.position = c(0.1, 0.8)) +
    labs(fill = "Resultado do RT-PCR")
dev.off()

