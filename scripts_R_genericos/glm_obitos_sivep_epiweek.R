library(ISOweek)
library(effects)
library(dplyr)
library(tidyr)
library(lubridate)

setwd("C:/Users/Tatiana/Documents/analises_covid/bases")

dados <- read.csv("SRAGH_2020_07_21.txt", sep=";", as.is=TRUE)
names(dados) <- tolower(names(dados))

###############COVID##########################

#####seleção municipio####

covid<- dados %>% 
  filter (co.mun.res == 355030) %>%
  filter (hospital==1) %>%
  filter (pcr.sars2==1 | classi.fin== 5)  %>% 
  filter (evolucao==1 | evolucao==2) %>%
  select (dt.sin.pri, evolucao) %>%
  mutate (dt.sin.pri = dmy(dt.sin.pri))
  

covid$week<-epiweek(covid$dt.sin.pri) ####semana epidemiológica começando no domingo

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

###tirando as últimas 3 semanas  da análise###

tabela<- tabela %>% filter (week<28)

###tranformando semana em fatcor

tabela$week<-as.factor(tabela$week)

#####fazendo o calculo do scfr##3

######glm bionmial###

model<-glm(cbind(obitos,sobre)~week, family=binomial (link="logit"), data= tabela)
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
  theme_classic2()+
  labs(title= "COVID", x="Semana Epidemiológica", y="proporção óbito")

############################SRAG######################

#####seleção municipio####

srag<- dados %>% 
  filter (co.mun.res == 355030) %>%
  filter (hospital==1) %>%
  filter (evolucao==1 | evolucao==2) %>%
  select (dt.sin.pri, evolucao) %>%
  mutate (dt.sin.pri = dmy(dt.sin.pri))


srag$week<-epiweek(srag$dt.sin.pri) ####semana epidemiológica começando no domingo

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

###tirando as últimas 3 semanas  da análise###

tabela2<- tabela2 %>% filter (week<28)

###tranformando semana em fatcor

tabela2$week<-as.factor(tabela2$week)

#####fazendo o calculo do scfr##3

######glm bionmial###

model2<-glm(cbind(obitos,sobre)~week, family=binomial (link="logit"), data= tabela2)
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
  theme_classic2()+
  labs(title= "SRAG", x="Semana Epidemiológica", y="proporção óbito")



