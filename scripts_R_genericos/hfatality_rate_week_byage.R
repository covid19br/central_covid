library(ISOweek)
library(effects)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
source("./nowcasting/fct/get.last.date.R")
source("./nowcasting/fct/read.sivep.R")

##PI: generalizei a leitura da SIVEP para rodar desde que seja de um diretorio que esteja no central covid
## setwd("C:/Users/Tatiana/Documents/analises_covid/bases")

## dados <- read.csv("SRAGH_2020_07_21.txt", sep=";", as.is=TRUE)
## names(dados) <- tolower(names(dados))

## Leitura dos dados: sivep residentes São Paulo
## Verifica qual SIVEP é mais recente entre a nacional e a do estado de SP
## Diretorios onde estao as duas siveps
dir.sp <- "./dados/estado_SP/SRAG_hospitalizados/dados/"
dir.br <- "./dados/SIVEP-Gripe/"
## Maior data de arquivos em cada diretorio
data.sp <- get.last.date(dir.sp)
data.br <- get.last.date(dir.sp)
## Seleciona o diretorio com a maior data
data.dir <- ifelse(as.Date(data.sp, "%Y_%m_%d") >= as.Date(data.br, "%Y_%m_%d"), dir.sp, dir.br)
## Geocode do municipio
# São Paulo - Capital
# geocode <- 355030
# UF<-"SP"
# name_city<-"São Paulo"
# # Rio Branco
# geocode<-120040
# UF<-"AC"
# name_city<-"Rio Branco"
# # Manaus
# geocode<-130260
# UF<-"AM"
# name_city<-"Manaus"
# # Macapá
# geocode<-160030
# UF<-"AP"
# name_city<-"Macapá"
# # Boa vista
# geocode<-140010
# UF<-"RR"
# name_city<-"Boa Vista"
# # Belém
# geocode<-150140
# UF<-"PA"
# name_city<-"Belém"
# # Palmas
# geocode<-172100
# UF<-"TO"
# name_city<-"Palmas"
# # São Luis
# geocode<-211130
# UF<-"MA"
# name_city<-"São Luís"
# # Maceió
# geocode<-270430
# UF<-"AL"
# name_city<-"Maceió"
# # Salvador
# geocode<-2927408
# UF<-"BA"
# name_city<-"Salvador"
# # Fortaleza
# geocode<-2304400
# UF<-"CE"
# name_city<-"Fortaleza"
# # Brasília
# geocode<-5300108
# UF<-"DF"
# name_city<-"Brasília"
# # Vitória
# geocode<-3205309
# UF<-"ES"
# name_city<-"Vitória"
# # Goiânia
# geocode<-5208707
# UF<-"GO"
# name_city<-"Goiânia"
# # Cuiabá
# geocode<-5103403
# UF<-"MT"
# name_city<-"Cuiabá"
# # Campo Grande
# geocode<-5002704
# UF<-"MS"
# name_city<-"Campo Grande"
# # Belo Horizonte
# geocode<-3106200
# UF<-"MG"
# name_city<-"Belo Horizonte"
# # João Pessoa
# geocode<-2507507
# UF<-"PB"
# name_city<-"João Pessoa"
# # Curitiba
# geocode<-4106902
# UF<-"PR"
# name_city<-"Curitiba"
# # Recife
# geocode<-2611606
# UF<-"PE"
# name_city<-"Recife"
# # Teresina
# geocode<-2211001
# UF<-"PI"
# name_city<-"Teresina"
# # Rio de Janeiro
# geocode<-3304557
# UF<-"RJ"
# name_city<-"Rio de Janeiro"
# # Natal
# geocode<-2408102
# UF<-"RN"
# name_city<-"Natal"
# # Porto Alegre
# geocode<-4314902
# UF<-"RS"
# name_city<-"Porto Alegre"
# # Porto Velho
# geocode<-1100205
# UF<-"RO"
# name_city<-"Porto Velho"
# # Florianópolis
# geocode<-4205407
# UF<-"SC"
# name_city<-"Florianópolis"
# # Aracaju
geocode<-2800308
UF<-"SE"
name_city<-"Aracaju"


data.dir<-dir.br
################################################################################
## Para fazer Manaus
##data.dir <- dir.br ## PI: escolha manual do diretorio de dados do Brasil
## geocode <- 1302603
################################################################################
## Leitura da ultima base do diretorio escolhido: já filtra município e residentes
dados <- read.sivep(dir = data.dir, escala = "municipio", 
                    # sigla = "SP",
                    geocode = geocode, data = get.last.date(data.dir))


#####CLASSIFICAÇAO ETARIA####

dados$nu_idade_n<-as.numeric(dados$nu_idade_n)


dados <- dados  %>% mutate(age_clas = case_when(nu_idade_n=1 & nu_idade_n<=19 ~  "age_0_19",
                                                nu_idade_n=20 & nu_idade_n<=39 ~ "age_20_39",
                                                nu_idade_n=40 & nu_idade_n<=59 ~ "age_40_59",
                                                nu_idade_n>=60 ~ "age_60"))


###############COVID##########################

##PI: com a read.sivep aqui só precisa fazer a seleção dos outros campos
 
###COVID####
covid<- dados %>% 
  filter (hospital==1) %>%
  filter (pcr_sars2==1 | classi_fin== 5)  %>% 
  filter (evolucao==1 | evolucao==2) %>%
  filter (!is.na(age_clas)) %>%
  select (dt_sin_pri, evolucao, age_clas) 

covid$week<-epiweek(covid$dt_sin_pri) ####semana epidemiológica começando no domingo

###SRAG#####

srag<- dados %>% 
  filter (hospital==1) %>%
  filter (evolucao==1 | evolucao==2) %>%
  filter (!is.na(age_clas)) %>%
  select (dt_sin_pri, evolucao, age_clas) 

srag$week<-epiweek(srag$dt_sin_pri) ####semana epidemiológica começando no domingo

#####AGREGANDO POR SEMANA EPIDEMIO E CLASSE ETÁRIA#####

## PI: outra maneira de fazer a tabela

####covid####

tabela  <-
  covid %>%
  group_by(week, age_clas) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao ==2))

##SRAG###

tabela2  <-
  srag %>%
  group_by(week, age_clas) %>%
  summarise(sobre = sum(evolucao == 1), obitos = sum(evolucao ==2))

###tirando as primeiras e as últimas 4 semanasda análise - ANALISAR CASO A CASO###

##COVID##
tabela<- tabela %>% filter (week<28 & week > 11) ## tirei tb semanas 1  9, que têm poucos casos

##SRAG###
tabela2<- tabela2 %>% filter (week<28 & week > 11) ## tirei tb semanas 1  9, que têm poucos casos

###tranformando semana em factor

##COVID##

tabela$week<-as.factor(tabela$week)
tabela$age_clas<-as.factor(tabela$age_clas)

##SRAG##

tabela2$week<-as.factor(tabela2$week)
tabela2$age_clas<-as.factor(tabela2$age_clas)

#####CALCULANDO IN HOSPITAL FATALITY RATE POR IDADE###

####################covid#########################

######glm bionmial###

model<-glm(cbind(obitos,sobre)~ week + age_clas , family=binomial (link="logit"), data= tabela) ## -1 na formula elimina o intecpto e aí cada cofieciente é o logito da CFR
anova(model, test="Chisq")

####calculando o predito###

new_data<-tabela[,c(1,2)]

## add fit and se.fit on the **link** scale

ilink <- family(model)$linkinv
predito <- bind_cols(new_data, setNames(as_tibble(predict(model, new_data, se.fit = TRUE)[1:2]),
                                        c('fit_link','se_link')))

## create the interval and backtransform
predito <- mutate(predito,
                  fit  = ilink(fit_link),
                  upper = ilink(fit_link + (2 * se_link)),
                  lower = ilink(fit_link - (2 * se_link)))
write.csv(predito, file = paste0("./scripts_R_genericos/IHFR_csv/", geocode,"_",name_city,"_covid_IHFR_glm.csv"), row.names = FALSE)

####################SRAG#########################

######glm bionmial###

model2<-glm(cbind(obitos,sobre)~ week + age_clas , family=binomial (link="logit"), data= tabela2) 
anova(model2, test="Chisq")

####calculando o predito###

new_data2<-tabela2[,c(1,2)]

## add fit and se.fit on the **link** scale
ilink2 <- family(model2)$linkinv
predito2 <- bind_cols(new_data2, setNames(as_tibble(predict(model2, new_data2, se.fit = TRUE)[1:2]),
                                          c('fit_link','se_link')))

## create the interval and backtransform
predito2 <- mutate(predito2,
                   fit  = ilink2(fit_link),
                   upper = ilink2(fit_link + (2 * se_link)),
                   lower = ilink2(fit_link - (2 * se_link)))
write.csv(predito, file = paste0("./scripts_R_genericos/IHFR_csv/", geocode,"_",name_city,"_SRAG_IHFR_glm.csv"), row.names = FALSE)
###PLOTS#####
# UF<-"SP"
###covid##

plot_covid<-ggplot(predito, aes(x=week, y=fit, group=age_clas))+
  geom_line(aes(color=age_clas))+
  scale_color_manual(values=c( "purple", "yellow", "blue", "red"))+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=age_clas), alpha=0.2)+
  scale_fill_manual(values=c("purple", "yellow", "blue", "red"))+
  theme_bw()+
  ggtitle(paste0(UF, "-", name_city), "COVID")

###srag####

plot_srag<-ggplot(predito2, aes(x=week, y=fit, group=age_clas))+
  geom_line(aes(color=age_clas))+
  scale_color_manual(values=c( "purple", "yellow", "blue", "red"))+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=age_clas), alpha=0.2)+
  scale_fill_manual(values=c("purple", "yellow", "blue", "red"))+
  theme_bw()+
  ggtitle(paste0(UF, "-", name_city), "SRAG")

ggarranged<-ggpubr::ggarrange (plot_covid,plot_srag, 
                   common.legend = TRUE, legend="bottom")
ggarranged
ggsave(ggarranged, filename = paste0("./scripts_R_genericos/IHFR_plots/plot_IHFR_", geocode,"_", name_city,".png"),
       dpi = 600, width = 9, height = 7)
