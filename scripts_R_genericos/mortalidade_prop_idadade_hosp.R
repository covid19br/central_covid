library(ISOweek)
library(effects)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(reshape)
source("./nowcasting/fct/get.last.date.R")
source("./nowcasting/fct/read.sivep.R")

##PI: generalizei a leitura da SIVEP para rodar desde que seja de um diretorio que esteja no central covid
## setwd("C:/Users/Tatiana/Documents/analises_covid/bases")

## dados <- read.csv("SRAGH_2020_07_21.txt", sep=";", as.is=TRUE)
## names(dados) <- tolower(names(dados))

## Leitura dos dados: sivep residentes S?o Paulo
## Verifica qual SIVEP ? mais recente entre a nacional e a do estado de SP
## Diretorios onde estao as duas siveps
dir.sp <- "./dados/estado_SP/SRAG_hospitalizados/dados/"
dir.br <- "./dados/SIVEP-Gripe/"
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
## Leitura da ultima base do diretorio escolhido: j? filtra munic?pio e residentes
dados <- read.sivep(dir = data.dir, escala = "municipio",
                    geocode = geocode, data = get.last.date(data.dir))


#####CLASSIFICA?AO ETARIA####

dados$age_clas<-dados$nu_idade_n


dados <- dados  %>% mutate(age_clas = case_when(nu_idade_n=1 & nu_idade_n<=19 ~  "age_0_19",
                                                nu_idade_n=20 & nu_idade_n<=39 ~ "age_20_39",
                                                nu_idade_n=40 & nu_idade_n<=59 ~ "age_40_59",
                                                nu_idade_n>=60 ~ "age_60"))

####

###############COVID##########################

covid<- dados %>% 
  filter (hospital==1) %>%
  filter (pcr_sars2==1 | classi_fin== 5)  %>% 
  filter (evolucao==1 | evolucao==2) %>%
  filter (!is.na(age_clas)) %>%
  select (dt_sin_pri, evolucao, age_clas) 

covid$week<-epiweek(covid$dt_sin_pri) ####semana epidemiol?gica come?ando no domingo


###################SRAG#################################


srag<- dados %>% 
  filter (hospital==1) %>%
  filter (evolucao==1 | evolucao==2) %>%
  filter (!is.na(age_clas)) %>%
  select (dt_sin_pri, evolucao, age_clas) 

srag$week<-epiweek(srag$dt_sin_pri) ####semana epidemiol?gica come?ando no domingo

#########AGREGANDO POR SEMANA EPIDEMIO E CLASSE ET?RIA#####

## PI: outra maneira de fazer a tabela

####covid####

tabela  <-
  covid %>%
  group_by(week, age_clas) %>%
  summarise(covid=n())


##SRAG###

tabela2  <-
  srag %>%
  group_by(week, age_clas) %>%
  summarise(srag=n())

###tirando as primeiras e as ?ltimas 4 semanasda an?lise - ANALISAR CASO A CASO###

##COVID##
tabela<- tabela %>% filter (week<28 & week > 11) ## tirei tb semanas 1  9, que t?m poucos casos

##SRAG###
tabela2<- tabela2 %>% filter (week<28 & week > 11) ## tirei tb semanas 1  9, que t?m poucos casos

###fazendo propor??o####

###COVID###

tabela<-cast(tabela, week~ age_clas)
tabela[is.na(tabela)]<-0
tabela$total<-rowSums(tabela[,2:5])
tabela$p_age_0_19<-(tabela$age_0_19*100)/tabela$total
tabela$p_age_20_39<-(tabela$age_20_39*100)/tabela$total
tabela$p_age_40_59<-(tabela$age_40_59*100)/tabela$total
tabela$p_age_60<-(tabela$age_60*100)/tabela$total

tabela_full<-tabela[,c(1,7,8,9,10)]

tabela_full<- tabela_full %>% 
  pivot_longer(cols = p_age_0_19:p_age_60)

###SRAG###

tabela2<-cast(tabela2, week~ age_clas)
tabela2[is.na(tabela2)]<-0
tabela2$total<-rowSums(tabela2[,2:5])
tabela2$p_age_0_19<-(tabela2$age_0_19*100)/tabela2$total
tabela2$p_age_20_39<-(tabela2$age_20_39*100)/tabela2$total
tabela2$p_age_40_59<-(tabela2$age_40_59*100)/tabela2$total
tabela2$p_age_60<-(tabela2$age_60*100)/tabela2$total

tabela_full2<-tabela2[,c(1,7,8,9,10)]

tabela_full2<- tabela_full2 %>% 
  pivot_longer(cols = p_age_0_19:p_age_60)

############IN hOSPITAL FATALITY RATE TOTAL########


####covid####

tabela3  <-
  covid %>%
  group_by(week) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao ==2))

##SRAG###

tabela4  <-
  srag %>%
  group_by(week) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao ==2))

###tirando as primeiras e as ?ltimas 4 semanasda an?lise - ANALISAR CASO A CASO###

##COVID##
tabela3<- tabela3 %>% filter (week<28 & week > 11) ## tirei tb semanas 1  9, que t?m poucos casos

##SRAG###
tabela4<- tabela4 %>% filter (week<28 & week > 11) ## tirei tb semanas 1  9, que t?m poucos casos

###tranformando semana em factor

##COVID##

tabela3$week<-as.factor(tabela3$week)

##SRAG##

tabela4$week<-as.factor(tabela4$week)


######glm bionmial###

###COVID#####

model<-glm(cbind(obitos,sobre)~ week , family=binomial (link="logit"), data= tabela3) ## -1 na formula elimina o intecpto e a? cada cofieciente ? o logito da CFR
anova(model, test="Chisq")

####calculando o predito###

new_data<-tabela3[,c(1,2)]

ilink <- family(model)$linkinv

predito <- bind_cols(new_data, setNames(as_tibble(predict(model, new_data, se.fit = TRUE)[1:2]),
                                        c('fit_link','se_link')))

predito <- mutate(predito,
                  fit  = ilink(fit_link),
                  upper = ilink(fit_link + (2 * se_link)),
                  lower = ilink(fit_link - (2 * se_link)))


####################SRAG#########################

######glm bionmial###

model2<-glm(cbind(obitos,sobre)~ week , family=binomial (link="logit"), data= tabela4) 
anova(model2, test="Chisq")

####calculando o predito###

new_data2<-tabela4[,c(1,2)]


ilink2 <- family(model2)$linkinv

predito2 <- bind_cols(new_data2, setNames(as_tibble(predict(model2, new_data2, se.fit = TRUE)[1:2]),
                                          c('fit_link','se_link')))

predito2 <- mutate(predito2,
                   fit  = ilink2(fit_link),
                   upper = ilink2(fit_link + (2 * se_link)),
                   lower = ilink2(fit_link - (2 * se_link)))

#####merge predito e tabela full###

##COVID

tabela_full$week<-as.factor(tabela_full$week)


##SRAG##

tabela_full2$week<-as.factor(tabela_full2$week)

######################PLOTS#######################################

###COVID##

plot_covid<-ggplot(data=predito, aes(x= week, y=fit, group = 1))+
  geom_bar(data = tabela_full, aes(fill=name, y=value, x=week), position="fill", stat="identity", alpha = 0.7)+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper, group = 1), alpha = 0.25)+
  scale_colour_brewer(palette = "Set3")+
  theme_bw()+
  # theme(legend.position = "none")+
  ggtitle(UF, "COVID")
plot_covid

plot_srag<-ggplot(data = predito2, aes(x=week, y=fit, group=1))+
  geom_bar(data = tabela_full2,aes(fill=name, y=value, x=week), position="fill", stat="identity", alpha = 0.7)+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper, group=1), alpha = 0.25)+
  scale_colour_brewer(palette = "Set3")+
  theme_bw()+
  # theme(legend.position = "none")+
  ggtitle(UF, "SRAG")
plot_srag

ggpubr::ggarrange(plot_covid, plot_srag, 
                  common.legend = TRUE, legend="bottom")







