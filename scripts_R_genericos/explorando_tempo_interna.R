library(ISOweek)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
#source("../nowcasting/fct/get.last.date.R")
#source("../nowcasting/fct/read.sivep.R")


## Leitura dos dados###
data.dir <- "../dados/SIVEP-Gripe/"
last.date <- get.last.date(data.dir)
dados <- read.sivep(dir = data.dir, escala = "pais", data = get.last.date(data.dir))

######colocando em classes et?rias########
dados$nu_idade_n <- as.numeric(dados$nu_idade_n)

dados <- dados  %>%
  mutate(age_clas = case_when(nu_idade_n = 1 & nu_idade_n <= 19 ~ "age_0_19",
                              nu_idade_n = 20 & nu_idade_n <= 39 ~ "age_20_39",
                              nu_idade_n = 40 & nu_idade_n <= 59 ~ "age_40_59",
                              nu_idade_n >= 60 ~ "age_60"))


###############COVID##########################

covid <- dados %>%
  filter(hospital == 1) %>%
  filter(pcr_sars2 == 1 | classi_fin == 5)  %>%
  filter(evolucao == 1 | evolucao == 2) %>%
  filter(!is.na(age_clas)) %>%
  filter (dt_sin_pri<=dt_interna)%>%
  filter (dt_sin_pri<=dt_entuti) %>%
  select(dt_sin_pri, dt_interna, dt_entuti, evolucao, age_clas, sg_uf)

###filtrando as 4 últimas semanas#######

# Classificando semana epidemiologica por estado
## Semana epidemiologica brasileira
covid$week <- epiweek(covid$dt_sin_pri) ####semana epidemiol?gica come?ando no domingo

covid<- covid %>% filter (week<28)%>%
                  filter (week>12)

###pegando data de internação###

covid$dt_interna<-as.character(covid$dt_interna)
covid$dt_entuti<-as.character(covid$dt_entuti)

covid$hospi<-ifelse(is.na(covid$dt_interna), covid$dt_entuti, covid$dt_interna)
covid$hospi<-ymd(covid$hospi)


####calulcando tempo do primeiro sintoma e data de internação###

covid$tempo_inter<-as.numeric(covid$hospi-covid$dt_sin_pri)



###tirando dados inconsistentes###

covid<-covid %>% filter (tempo_inter<360) %>%
                 filter (tempo_inter>=0)

###filtrando estados##

estados<-c("SP", "RS", "SC", "PR", "MG", "RJ", "BA", "AM", "MA", "PA", "AL", "PE", "CE", "ES", "PI", "PB","GO", "MS", "MT")

df_covid<- covid %>% filter (sg_uf %in% estados)

####fazendo uma média de internação e primeiros sintomas por estado###

tabela<- covid %>% 
        group_by(sg_uf) %>%
        summarise(mean=mean(tempo_inter, na.rm=TRUE), sd=sd(tempo_inter, na.rm = TRUE))

tabela2<- covid %>% 
  group_by(sg_uf, age_clas) %>%
  summarise(mean=mean(tempo_inter, na.rm=TRUE), sd=sd(tempo_inter, na.rm = TRUE))

tabela3<- covid %>% 
  group_by(sg_uf, week) %>%
  summarise(mean=mean(tempo_inter, na.rm=TRUE), sd=sd(tempo_inter, na.rm = TRUE))%>%
  as.data.frame()

class(tabela3)

df_covid <- covid %>%
  filter(sg_uf %in% estados)


ggplot(df_covid, aes(x = factor(sg_uf), y = tempo_inter))+ 
  geom_boxplot(trim=FALSE, fill="gray") + 
  xlab ("Estados")+
  ylab ("dias entre primeiros sintomas e internação")+
  #stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")+
  coord_cartesian(ylim = c(0, 30))+
  theme_bw()

ggplot(df_covid, aes(x = factor(age_clas), y = tempo_inter))+ 
  facet_wrap(~sg_uf, ncol=3)+
  geom_boxplot(fill="gray") + 
  xlab ("Faixa etária")+
  ylab ("dias entre primeiros sintomas e internação")+
  #stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")+
  coord_cartesian(ylim = c(0, 25))+
  theme_bw()

#ggplot(covid, aes(x = tempo_inter))+ 
#  geom_histogram(aes(y=..density..))+
#  facet_wrap(~sg_uf, ncol=4)+
#  coord_cartesian(ylim = c(0, 0.15))+
#  theme_bw()

tabela3<- tabela3 %>% filter (sg_uf%in% estados)

ggplot(tabela3, aes(x= week, y=mean))+
  geom_line()+
  facet_wrap(~sg_uf, ncol=4)+
  xlab ("Semana de primeiro sintoma")+
  ylab ("dias entre primeiros sintomas e internação")+
  theme_bw()
  
explorando<- df_covid %>% filter (sg_uf=="MS")%>%
                          filter (week==18)
                           

#write.csv(tabela, file = paste0("output/dados/", "summary_covid_IFHR","_", last.date,".csv"),
  #        row.names = FALSE)
#write.csv(tabela2, file = paste0("output/dados/", "summary_srag_IFHR","_", last.date,".csv"),
  #        row.names = FALSE)

