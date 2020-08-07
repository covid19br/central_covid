library(ISOweek)
library(effects)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(viridis)
library(bbmle)
source("../nowcasting/fct/get.last.date.R")
source("../nowcasting/fct/read.sivep.R")


## Leitura dos dados###
data.dir <- "../dados/SIVEP-Gripe/"
dados <- read.sivep(dir = data.dir, escala = "pais", data = get.last.date(data.dir))#### eu analisei com a base do dia 29

######colocando em classes etárias########

dados$nu_idade_n<-as.numeric(dados$nu_idade_n)

dados <- dados  %>% mutate(age_clas = case_when(nu_idade_n=1 & nu_idade_n<=19 ~  "age_0_19",
                                                nu_idade_n=20 & nu_idade_n<=39 ~ "age_20_39",
                                                nu_idade_n=40 & nu_idade_n<=59 ~ "age_40_59",
                                                nu_idade_n>=60 ~ "age_60"))


####Criando categoria UTI e LEITOS#####

dados<-dados %>% mutate (local_int = case_when (uti==1|!is.na(dt_entuti)|!is.na(dt_saiduti) ~ "UTI",
                                                uti==2 ~ "Leito"))


###############COVID##########################

covid<- dados %>% 
  filter (hospital==1) %>%
  filter (pcr_sars2==1 | classi_fin== 5)  %>% 
  filter (evolucao==1 | evolucao==2) %>%
  filter (!is.na(age_clas)) %>%
  select (dt_sin_pri, evolucao, age_clas, local_int, sg_uf) 


covid$week<-epiweek(covid$dt_sin_pri) ####semana epidemiológica começando no domingo

###################SRAG#################################

srag<- dados %>% 
  filter (hospital==1) %>%
  filter (evolucao==1 | evolucao==2) %>%
  filter (!is.na(age_clas)) %>%
  select (dt_sin_pri, evolucao, age_clas, local_int ,sg_uf)

srag$week<-epiweek(srag$dt_sin_pri) ####semana epidemiológica começando no domingo


####################COM TODOS OS ESTADOS ########################

################selecionando alguns estados##########################

##COVID##

tabela  <-
  covid %>%
  group_by(week, age_clas, local_int, sg_uf) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao ==2))

##SRAG###

tabela2  <-
  srag %>%
  group_by(week, age_clas, local_int, sg_uf) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao ==2))

################################################################################
##PI:  Graficos exploratorios
################################################################################
## Trajetoria do N de obitos em funcao do N total de casos
png("IHFR_plots/Estados/trajetorias_obitosXcasos.png", width = 900)
tabela %>%
    filter(week<28&!is.na(sg_uf)) %>%
    group_by(week, sg_uf) %>%
    summarise(ob = sum(obitos), sob=sum(sobre)) %>%
    mutate(N = ob+sob) %>% 
    ggplot(aes(N, ob)) +
    geom_abline(slope=0.5, intercept=0, col="red", lty=2) +
    geom_path(aes(color=week), size=1.1) +
    scale_color_viridis() +
    facet_wrap(~sg_uf, scales = "free") +
    theme_bw() +
    xlab("Total de casos") +
    ylab("N de óbitos")
dev.off()

## Letalidade em funcao do n de semanas após a semana de maior n de casos (semana zero)
## Tabela com a semana de maior n de casos em cada estado
tmp <- tabela %>%
    filter(week<28&!is.na(sg_uf)) %>%
    group_by(sg_uf, week) %>%
    summarise(ob = sum(obitos), sob=sum(sobre)) %>%
    mutate(N = ob+sob, Nmax = N ==max(N)) %>%
    ungroup() %>%
    filter(Nmax==TRUE) %>%
    select(sg_uf, week)

## O plot    
tabela %>%
    filter(week<28&!is.na(sg_uf)) %>%
    group_by(week, sg_uf) %>%
    summarise(ob = sum(obitos), sob=sum(sobre)) %>%
    mutate(N = ob+sob) %>%
    merge(tmp, by = "sg_uf") %>%
    mutate(week2 = week.x - week.y, p.ob = ob/N) %>%
    filter(N>9) %>%
    ggplot(aes(week2, p.ob)) +
    geom_point() +
    facet_wrap(~sg_uf) +
    theme_bw() +
    geom_vline(xintercept = 0, lty=2, color="red") +
    xlab("Semanas após a semana de máximo de casos") +
    ylab("Proporcao de óbitos")

#####################10 ESTADOS COM MAIS CASOS###############################

############Selecionando alguns estados e agregando variáveis########

####covid####

tabela3  <-
  covid %>%
  filter (sg_uf== "SP" |sg_uf=="RJ"| sg_uf=="CE"| sg_uf=="PA"| sg_uf=="PE"| sg_uf=="MG"| sg_uf=="AM"|
            sg_uf=="BA"| sg_uf=="RS"| sg_uf=="PR") %>%
  group_by(week, age_clas, local_int, sg_uf) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao ==2))

##SRAG###

tabela4  <-
  srag %>%
  filter (sg_uf== "SP" |sg_uf=="RJ"| sg_uf=="CE"| sg_uf=="PA"| sg_uf=="PE"| sg_uf=="MG"| sg_uf=="AM"|
            sg_uf=="BA"| sg_uf=="RS"| sg_uf=="PR") %>%
  group_by(week, age_clas, local_int, sg_uf) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao ==2))


############################################################

###tirando as primeiras e as últimas 4 semanasda análise - ANALISAR CASO A CASO###

##COVID##
tabela<- tabela %>% filter (week<28 & week > 11) ## tirei tb semanas 1  9, que têm poucos casos

##SRAG###
tabela2<- tabela2 %>% filter (week<28 & week > 11) ## tirei tb semanas 1  9, que têm poucos casos


#####10 estados#####

#COVID##
tabela3<- tabela3 %>% filter (week<28 & week > 11) 

##SRAG###
tabela4<- tabela4 %>% filter (week<28 & week > 11) 


###tranformando semana em factor

##COVID##

tabela$week<-as.factor(tabela$week)
tabela$age_clas<-as.factor(tabela$age_clas)
tabela$local_int<-as.factor(tabela$local_int)
tabela$sg_uf<-as.factor(tabela$sg_uf)

##SRAG##

tabela2$week<-as.factor(tabela2$week)
tabela2$age_clas<-as.factor(tabela2$age_clas)
tabela2$local_int<-as.factor(tabela2$local_int)
tabela2$sg_uf<-as.factor(tabela2$sg_uf)

#####10 estados####


#COVID##

tabela3$week<-as.factor(tabela3$week)
tabela3$age_clas<-as.factor(tabela3$age_clas)
tabela3$local_int<-as.factor(tabela3$local_int)
tabela3$sg_uf<-as.factor(tabela3$sg_uf)

##SRAG##

tabela4$week<-as.factor(tabela4$week)
tabela4$age_clas<-as.factor(tabela4$age_clas)
tabela4$local_int<-as.factor(tabela4$local_int)
tabela4$sg_uf<-as.factor(tabela4$sg_uf)

##############GLM#######################################

#####################TODOS OS ESTADOS###########################################

###########comentários########

### eu achei que os plots dos resíduos dos modelos mais explicativos com todos os estados não ficaram legais## 
###Dai tentei ajustar o modelo com os 10 estados que tem mais casos covid e srag e parece que ficou mais razoável.##########

####################COVID#########################

###modelo1 cheio###

model1<-glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf + sg_uf:age_clas + week:age_clas + week:sg_uf + sg_uf:local_int + week:local_int,
            family=binomial (link="logit"), data= tabela) 

summary(model1)


####tirando a interação semana e IDADE####


model2<-glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf + sg_uf:age_clas + week:sg_uf + sg_uf:local_int + week:local_int,
            family=binomial (link="logit"), data= tabela) 

anova(model2, model1, test="Chisq") ####deu significativo, mantem o mais complexo

summary(model1)
plot(model1)


################################################################################
## PI: Exemplo de seleção de modelos
################################################################################
## Note que nestes modelos a variavel week está como contínua. Neste casos é posíivel que um termo quadrático I(week^2)
## seja necessario, para permitir aumentos seguidos de quedas em função
## 1. Ajuste todos os modelos
## Cheio
m.full <- glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf +
                 sg_uf:age_clas + week:age_clas +
                 week:sg_uf + sg_uf:local_int + week:local_int,
            family=binomial (link="logit"), data= tabela) 
## Apenas efeitos aditivos e interacao entre semana e UF (ou seja, o aumento em funcao da semana varia entre UFs)
m.1 <- glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf  + week:sg_uf,
            family=binomial (link="logit"), data= tabela) 
## Com todas as interecaos menos interacoes de semana com local de internacao
m.2 <- glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf +
                 sg_uf:age_clas + week:age_clas +
                 week:sg_uf + sg_uf:local_int,
            family=binomial (link="logit"), data= tabela) 
## Com todas as interecaos menos interacoes de semana com idade e com local de internacao
m.3 <- glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf +
               sg_uf:age_clas +
                 week:sg_uf + sg_uf:local_int ,
           family=binomial (link="logit"), data= tabela) 
## Igual o acima, mas sem interacao entre UF e local de internacao
m.4 <- glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf +
               sg_uf:age_clas +
                 week:sg_uf,
           family=binomial (link="logit"), data= tabela) 
## Comparando estes modelos com AIC
AICctab(m.full, m.1, m.2, m.3, m.4)


####################SRAG#########################

######glm bionmial###

###modelo1 cheio###

Smodel1<-glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf + sg_uf:age_clas + week:age_clas + week:sg_uf + sg_uf:local_int + week:local_int,
             family=binomial (link="logit"), data= tabela2)

####tiando a interação semana e IDADE#### 

Smodel2<-glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf + sg_uf:age_clas + week:sg_uf +  sg_uf:local_int + week:local_int,
             family=binomial (link="logit"), data= tabela2)


anova(Smodel2, Smodel1, test="Chisq") #### deu significativo

summary(Smodel1)


plot(Smodel1)

####################10 ESTADOS COM MAIS CASOS##########################################

####################COVID#########################

###modelo1 cheio###

sub_model1<-glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf + sg_uf:age_clas + week:age_clas + week:sg_uf + sg_uf:local_int + week:local_int,
                family=binomial (link="logit"), data= tabela3) 

summary(sub_model1)

####tiando a interação semana e IDADE####


sub_model2<-glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf + sg_uf:age_clas + week:sg_uf + sg_uf:local_int + week:local_int,
                family=binomial (link="logit"), data= tabela3) ## -1 na formula elimina o intecpto e aí cada cofieciente é o logito da CFR


anova(sub_model2, sub_model1, test="Chisq") ####deu significativo

####tiando a interação semana e local de internação####


sub_model3<-glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf + sg_uf:age_clas + week:sg_uf + sg_uf:local_int,
                family=binomial (link="logit"), data= tabela3) ## -1 na formula elimina o intecpto e aí cada cofieciente é o logito da CFR


anova(sub_model3, sub_model2, test="Chisq") ####Significativo

summary(sub_model2)

plot(sub_model2)


####################SRAG#########################

######glm bionmial###

###modelo1 cheio###

sub_Smodel1<-glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf + sg_uf:age_clas + week:age_clas + week:sg_uf + sg_uf:local_int + week:local_int,
                 family=binomial (link="logit"), data= tabela4) ## -1 na formula elimina o intecpto e aí cada cofieciente é o logito da CFR

####tiando a interação semana e IDADE####

sub_Smodel2<-glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf + sg_uf:age_clas + week:sg_uf + sg_uf:local_int + week:local_int,
                 family=binomial (link="logit"), data= tabela4) ## -1 na formula elimina o intecpto e aí cada cofieciente é o logito da CFR

anova(sub_model1, teste= "Chisq")

anova(sub_Smodel2, sub_Smodel1, test="Chisq") #### não deu significativo


################tirando semana e local de internação###

sub_Smodel3<-glm(cbind(obitos,sobre)~ week + age_clas + local_int + sg_uf + sg_uf:age_clas + week:sg_uf + sg_uf:local_int,
                 family=binomial (link="logit"), data= tabela4) ## -1 na formula elimina o intecpto e aí cada cofieciente é o logito da CFR


anova(sub_Smodel3, sub_Smodel2, test="Chisq") ####Significativo = mantem complexo


summary(sub_Smodel2)

plot(sub_Smodel2)











