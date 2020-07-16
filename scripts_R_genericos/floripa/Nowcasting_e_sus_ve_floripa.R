library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(ggplot2)
library(NobBS)
library(R.utils)
library(readr)


setwd("C:/Users/Tatiana/Documents/analises_covid/bases")

#######Parâmetros#######

file.name="covid_anonimizado_2.csv"
file.name2="esus-ve_sc-2020_06_09.csv.bz2"
loc <- "UTF-8"
sep=","
estado.name="santa catarina"
mun.name="florianópolis"

guess_encoding(file.name)

dados<-read.csv("covid_anonimizado_2.csv",sep = ",", encoding = "ISO-8859-2" )

dados$Data.do.iní.cio.dos.sintomas<-mdy(dados$Data.do.iní.cio.dos.sintomas)
dados$Data.da.liberação.do.resultado<-mdy(dados$Data.da.liberação.do.resultado)
dados$Data.da.notificação<-mdy(dados$Data.da.notificação)

str(dados)


##selecionando o muncipio###

dados_mun<- dados %>% filter (Município.de.notificação=="FLORIANOPOLIS")

###gera.nowcasting_e_SUS_VE#####

##selecionando os dados para o nowcasting#####

table(dados_mun$Resultado.do.teste)

dados_mun<- dados_mun %>% filter (Resultado.do.teste=="POSITIVO") %>%
                          mutate (Data.da.liberação.do.resultado= pmax(Data.da.liberação.do.resultado, Data.da.notificação, na.rm= TRUE)) %>%
                          select (Data.da.liberação.do.resultado, Data.do.iní.cio.dos.sintomas)



##nowcasting dados mun##

trim.now=4
window=40

dados.now <- NobBS(
  data = dados_mun,
  now = max(dados_mun$Data.do.iní.cio.dos.sintomas), #- trim.now,
  onset_date = "Data.do.iní.cio.dos.sintomas",
  report_date = "Data.da.liberação.do.resultado",
  units = "1 day",
  moving_window = window)

now.dados.mun<-dados.now$estimates

now.dados.mun$estimate<-as.integer(now.dados.mun$estimate)
now.dados.mun$lower<-as.integer(now.dados.mun$lower)
now.dados.mun$upper<-as.integer(now.dados.mun$upper)


###agregando casos por data de sintoma############

casos_mun<- dados_mun %>% group_by(Data.do.iní.cio.dos.sintomas) %>%
  summarise(Casos=n())  %>%
  as.data.frame()

#####juntando os casos estimados com reais#####

casos_mun<- casos_mun %>% filter (Data.do.iní.cio.dos.sintomas < min(now.dados.mun$onset_date))
names(casos_mun)<-c("onset_date","estimate")
casos_mun$lower<-casos_mun$estimate
casos_mun$upper<-casos_mun$estimate
now.dados.mun<- now.dados.mun %>% select(estimate,onset_date, lower, upper)
casos.now.mun<-bind_rows(casos_mun,now.dados.mun)


###########fazendo a cumulativa####################

casos.now.mun$estimate<-cumsum(casos.now.mun$estimate)
casos.now.mun$lower<-cumsum(casos.now.mun$lower)
casos.now.mun$upper<-cumsum(casos.now.mun$upper)


######cortando data para o gráfico###########

ggplot(casos.now.mun, aes(x=onset_date,y=estimate))+
  geom_line(data= casos2_mun, aes(x=dataInicioSintomas, y=cum_casos), col="red")+
  geom_line(data=casos.now.mun, aes(x=onset_date, y=estimate), size=1)+
  #geom_line(data=estimates.now.esus_jp, aes(x=onset_date, y=estimate, col="Nowcasting"), size=1)+
  geom_ribbon(data=casos.now.mun, aes(ymin=lower, ymax=upper),
              fill = RColorBrewer::brewer.pal(3, "Set1")[2], alpha = 0.5) +
  scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")[1:2]) +
  #geom_line(data=srag_for, aes(x=dt_evoluca, y=smotth), size=2, alpha=0.5)+
  xlab("Data primeiro Sintoma") +
  ylab("Número novos casos leves/dia") +
  plot.formatos+
  ggtitle("Casos leves de COVID em manaus - AM - trim2")+
  theme(legend.position = c(0.2, 0.8))


casos.now.mun2




