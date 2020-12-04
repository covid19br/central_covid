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
        select(datainiciosintomas, datateste,
               estadoteste, datateste, tipoteste, resultadoteste)
}


## Leitura do E-SUS do estado,  data de 28/11/2020
sampa1 <- f1(paste0(data.dir,"esus-ve_sp-1-2020_11_28.csv.bz2"))
sampa2 <- f1(paste0(data.dir,"esus-ve_sp-2-2020_11_28.csv.bz2"))
sampa3 <- f1(paste0(data.dir,"esus-ve_sp-3-2020_11_28.csv.bz2"))
sampa4 <- f1(paste0(data.dir,"esus-ve_sp-4-2020_11_28.csv.bz2"))
sampa5 <- f1(paste0(data.dir,"esus-ve_sp-5-2020_11_28.csv.bz2"))

sampa.11.28 <-  rbind(sampa1, sampa2, sampa3, sampa4, sampa5)

rm(sampa1, sampa2, sampa3, sampa4, sampa5)

save.image()

## Leitura do E-SUS do estado,  ultima data disponivel
ultima.data <- get.last.date(data.dir)
sampa1 <- f1(paste0(data.dir,"esus-ve_sp-1-",ultima.data,".csv.bz2"))
sampa2 <- f1(paste0(data.dir,"esus-ve_sp-2-",ultima.data,".csv.bz2"))
sampa3 <- f1(paste0(data.dir,"esus-ve_sp-3-",ultima.data,".csv.bz2"))
sampa4 <- f1(paste0(data.dir,"esus-ve_sp-4-",ultima.data,".csv.bz2"))
sampa5 <- f1(paste0(data.dir,"esus-ve_sp-5-",ultima.data,".csv.bz2"))

sampa.ultima <-  rbind(sampa1, sampa2, sampa3, sampa4, sampa5)

rm(sampa1, sampa2, sampa3, sampa4, sampa5)

save.image()

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
    theme(legend.position = c(0.1, 0.8))   
dev.off()
