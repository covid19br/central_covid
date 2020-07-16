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

file.name="esus-ve_sc-2020_07_03.csv.bz2"
file.name2=
loc <- "UTF-8"
sep=","
estado.name="santa catarina"
mun.name="florianópolis"

####TATI ISTOOOO VAI PEGAR O MELHOR ENCODING MESMO QUE NAO SEJA CONDIFENCIA == 1
loc <- guess_encoding(file.name, n_max = 500) %>%
  arrange(loc, desc(confidence)) %>%
  pull(encoding[1])
##########

guess_encoding(file.name)

###############Leitura dos dados ################

dados <- readr::read_delim(file = file.name,
                           delim = ";",
                           #col_types = cols.list,
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           locale = readr::locale(encoding = "ISO-8859-1"))

dados2 <- readr::read_delim(file = file.name2,
                           delim = ";",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           #n_max = 1,
                           locale = readr::locale(encoding = loc))



#precisa tirar as linhas problematicas

index_nada <- problems(dados)$row
dados <- dados[-index_nada,]#tirar os erros de leitura

index_nada <- problems(dados2)$row
dados2 <- dados2[-index_nada,]#tirar os erros de leitura


####read.esus.ve#####

###convertando a data####

dados$dataInicioSintomas<-substr(dados$dataInicioSintomas,1,10)
dados$dataEncerramento<-substr(dados$dataEncerramento,1,10)
dados$dataNotificacao<-substr(dados$dataNotificacao,1,10)
dados$dataTeste<-substr(dados$dataTeste,1,10)

dados2$dataInicioSintomas<-substr(dados2$dataInicioSintomas,1,10)
dados2$dataEncerramento<-substr(dados2$dataEncerramento,1,10)
dados2$dataNotificacao<-substr(dados2$dataNotificacao,1,10)
dados2$dataTeste<-substr(dados2$dataTeste,1,10)

dados <- dados%>% mutate_at(vars(starts_with("data")), funs(ymd))
dados2 <- dados2%>% mutate_at(vars(starts_with("data")), funs(ymd))


##selecionando o muncipio###

dados_mun<- dados %>% filter (tolower(estado)==estado.name) %>%
             filter (tolower(municipio)==mun.name)

dados2_mun<- dados2 %>% filter (tolower(estado)==estado.name) %>%
  filter (tolower(municipio)==mun.name)

###gera.nowcasting_e_SUS_VE#####

##selecionando os dados para o nowcasting#####


dados_mun<- dados_mun %>% filter (resultadoTeste=="Positivo" | str_detect(classificacaoFinal, "Confirma")) %>%
                  select (dataInicioSintomas, dataNotificacao, dataTeste, dataEncerramento) %>%
                   mutate (dataEncerramento = pmax(dataTeste, dataNotificacao, dataEncerramento, na.rm=TRUE))

dados_mun<-as.data.frame(dados_mun)

dados2_mun<- dados2_mun %>% filter (resultadoTeste=="Positivo" | str_detect(classificacaoFinal, "Confirma")) %>%
  select (dataInicioSintomas, dataNotificacao, dataTeste, dataEncerramento) %>%
  mutate (dataEncerramento = pmax(dataTeste, dataNotificacao, dataEncerramento, na.rm=TRUE))

dados2_mun<-as.data.frame(dados2_mun)

#####tranformando em data###

##nowcasting dados mun##

trim.now=4
window=40

dados.now <- NobBS(
    data = dados_mun,
    now = max(dados_mun$dataEncerramento) - trim.now,
    onset_date = "dataInicioSintomas",
    report_date = "dataEncerramento",
    units = "1 day",
    #max_D =40)
    moving_window = window)
    
    
now.dados.mun<-dados.now$estimates

now.dados.mun$estimate<-as.integer(now.dados.mun$estimate)
now.dados.mun$lower<-as.integer(now.dados.mun$lower)
now.dados.mun$upper<-as.integer(now.dados.mun$upper)


###agregando casos por data de sintoma############

casos_mun<- dados_mun %>% group_by(dataInicioSintomas) %>%
  summarise(Casos=n())  %>%
  as.data.frame()


casos2_mun<- dados2_mun %>% group_by(dataInicioSintomas) %>%
  summarise(Casos=n())  %>%
  as.data.frame()

casos2_mun$cum_casos<-cumsum(casos2_mun$Casos)

#####juntando os casos estimados com reais#####

casos_mun<- casos_mun %>% filter (dataInicioSintomas < min(now.dados.mun$onset_date))
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
  #geom_line(data= casos2_mun, aes(x=dataInicioSintomas, y=cum_casos), col="red")+
  geom_line(data=casos.now.mun, aes(x=onset_date, y=estimate), size=1)+
  #geom_line(data=estimates.now.esus_jp, aes(x=onset_date, y=estimate, col="Nowcasting"), size=1)+
  geom_ribbon(data=casos.now.mun, aes(ymin=lower, ymax=upper),
             fill = RColorBrewer::brewer.pal(3, "Set1")[2], alpha = 0.5) +
  scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")[1:2]) +
  #geom_line(data=srag_for, aes(x=dt_evoluca, y=smotth), size=2, alpha=0.5)+
  xlab("Data primeiro Sintoma") +
  ylab("Número acumulado de casos leves") +
  plot.formatos+
  ggtitle("Nowcasting casos leves Florianópolis")+
  theme(legend.position = c(0.2, 0.8))






