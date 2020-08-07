library(ISOweek)
library(effects)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
source("../../nowcasting/fct/get.last.date.R")
source("../../nowcasting/fct/read.sivep.R")

##PI: generalizei a leitura da SIVEP para rodar desde que seja de um diretorio que esteja no central covid
## setwd("C:/Users/Tatiana/Documents/analises_covid/bases")

## dados <- read.csv("SRAGH_2020_07_21.txt", sep=";", as.is=TRUE)
## names(dados) <- tolower(names(dados))

## Leitura dos dados: sivep residentes São Paulo
## Verifica qual SIVEP é mais recente entre a nacional e a do estado de SP
## Diretorios onde estao as duas siveps
dir.sp <- "../dados/estado_SP/SRAG_hospitalizados/dados/"
dir.br <- "../dados/SIVEP-Gripe/"
## Maior data de arquivos em cada diretorio
data.sp <- get.last.date(dir.sp)
data.br <- get.last.date(dir.sp)
## Seleciona o diretorio com a maior data
data.dir <- ifelse(as.Date(data.sp, "%Y_%m_%d") >= as.Date(data.br, "%Y_%m_%d"), dir.sp, dir.br)
## Geocode do municipio
geocode <- 355030
################################################################################
## Para fazer Manaus
##data.dir <- dir.br ## PI: escolha manual do diretorio de dados do Brasil
## geocode <- 1302603
################################################################################
## Leitura da ultima base do diretorio escolhido: já filtra município e residentes
dados <- read.sivep(dir = data.dir, escala = "municipio",
                    geocode = geocode, data = get.last.date(data.dir))

###############COVID##########################

##PI: com a read.sivep aqui só precisa fazer a seleção dos outros campos
covid<- dados %>% 
  filter (hospital==1) %>%
  filter (pcr_sars2==1 | classi_fin== 5)  %>% 
  filter (evolucao==1 | evolucao==2) %>%
  select (dt_sin_pri, evolucao) 

  
covid$week<-epiweek(covid$dt_sin_pri) ####semana epidemiológica começando no domingo

###selecionando óbitos e não óbitos####

covid_obito<- covid %>% filter (evolucao==2)

covid_sobre<- covid %>% filter (evolucao==1)

###Agregando por semana de sintomas####

covid_obito2 <-  covid_obito %>% group_by(week) %>%
  summarise(obitos=n())  %>%
  as.data.frame()

covid_sobre2<- covid_sobre %>% group_by(week) %>%
  summarise(sobre=n())  %>%
  as.data.frame()

###juntando em uma data frame só###

tabela<-inner_join(covid_obito2, covid_sobre2)

## PI: outra maneira de fazer a tabela
tabela  <-
    covid %>%
    group_by(week) %>%
    summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao ==2))

###tirando as últimas 3 semanas  da análise###

tabela<- tabela %>% filter (week<28 & week > 9) ## tirei tb semanas 1  9, que têm poucos casos

###tranformando semana em fatcor

tabela$week<-as.factor(tabela$week)

#####fazendo o calculo do scfr##3

######glm bionmial###

model<-glm(cbind(obitos,sobre)~week -1 , family=binomial (link="logit"), data= tabela) ## -1 na formula elimina o intecpto e aí cada cofieciente é o logito da CFR
anova(model, test="Chisq")

summary(model)

##predito pelo modelo
predito<- effect("week", model)
predito <- as.data.frame(predito)

predito$week<-as.numeric(as.character(predito$week))

###tabela#####

ggplot(data = predito, aes(x = week, y=fit, ymin= lower, ymax=upper)) +
    geom_pointrange(col="blue")+
    geom_line (col="blue")+
    ##theme_classic2()+ #Pi: não tenho o pacote
    theme_bw() +
    labs(title= "COVID", x="Semana Epidemiológica", y="proporção óbito")

############################SRAG######################

#####seleção municipio####

srag<- dados %>% 
  filter (hospital==1) %>%
  filter (evolucao==1 | evolucao==2) %>%
  select (dt_sin_pri, evolucao)

srag$week<-epiweek(srag$dt_sin_pri) ####semana epidemiológica começando no domingo

###selecionando óbitos e não óbitos####

srag_obito<- srag %>% filter (evolucao==2)

srag_sobre<- srag %>% filter (evolucao==1)

###Agregando por semana de sintomas####

srag_obito2 <-  srag_obito %>% group_by(week) %>%
  summarise(obitos=n())  %>%
  as.data.frame()

srag_sobre2<- srag_sobre %>% group_by(week) %>%
  summarise(sobre=n())  %>%
  as.data.frame()

###juntando em uma data frame só###

tabela2<-inner_join(srag_obito2, srag_sobre2)

## PI: alternativa
tabela2 <-
    srag %>%
    group_by(week) %>%
    summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao ==2))
    
###tirando as últimas 3 semanas  da análise###

tabela2<- tabela2 %>% filter (week<28 & week >9)

###tranformando semana em fatcor

tabela2$week<-as.factor(tabela2$week)

#####fazendo o calculo do scfr##3

######glm bionmial###

model2<-glm(cbind(obitos,sobre)~week-1, family=binomial (link="logit"), data= tabela2)
anova(model2, test="Chisq")

summary(model2)

##predito pelo modelo
predito2<- effect("week", model2)
predito2 <- as.data.frame(predito2)

predito2$week<-as.numeric(as.character(predito2$week))

###tabela#####

ggplot(data = predito2, aes(x = week, y=fit, ymin= lower, ymax=upper)) +
    geom_pointrange(col="blue")+
    geom_line (col="blue")+
    ##theme_classic2()+
    theme_bw() +
    labs(title= "SRAG", x="Semana Epidemiológica", y="proporção óbito")


## Juntando as duas estimativas em um só plot

tudo  <-  merge(predito, predito2, by="week", suffixes=c(".covid", ".srag"))

tudo %>%
    ggplot(aes(x = week)) +
    geom_line(aes(y=fit.covid, col="Confirmados"))+
    geom_ribbon(aes(ymin = lower.covid, ymax = upper.covid), fill="red", alpha =0.2) +
    geom_line(aes(y=fit.srag, col="SRAG"))+
    geom_ribbon(aes(ymin = lower.srag, ymax = upper.srag), fill = "blue", alpha =0.2) +
    ylab("CFR") +
    xlab("Semana epidemiológica") +
    scale_colour_manual(values = c("red","blue"))
