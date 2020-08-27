###############
### Diadema ###
###############

## Loading de pacotes necessários ##  
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(NobBS)){install.packages("NobBS"); library(NobBS)}
if(!require(rprojroot)){install.packages("rprojroot"); library(rprojroot)}
if(!require(readxl)){install.packages("readxl"); library(readxl)}
if(!require(textclean)){install.packages("textclean"); library(textclean)}
if(!require(ggpubr)){install.packages("ggpubr"); library(ggpubr)}

source("beta.summary.R") ## função necessária, deve estar no mesmo diretório ##

diadema<-read_csv("diadema.csv") ## ATENÇÃO! ESSA CSV FOI MODIFICADA PARA NÃO CONTER CARACTERES NÃO ASCII, IMPORTANTE MANTER ASSIM ##
names(diadema)<-tolower(names(diadema)) ## Colocando os nomes das colunas em caixa-baixa ##

diadema_casos<-diadema %>% 
  filter(!is.na(data_pri_sin) & ## retirando datas de 1º sintomas com NA
           !is.na(data_notificacao) & ## retirando datas de notificação com NA
           is.na(obito)) %>% ## Selecionando quem NÃO é óbito 
  mutate(data_da_coleta = as.Date(data_da_coleta, "%d/%m/%Y"), ## Transformando em formato data padronizado
         data_pri_sin = as.Date(data_pri_sin, "%d/%m/%Y"),
         data_do_obito = as.Date(data_do_obito, "%d/%m/%Y"),
         data_notificacao = as.Date(data_notificacao, "%d/%m/%Y"),
         atraso = as.numeric(data_notificacao - data_pri_sin)) %>% ## calculando o atraso entre 1º sintomas e notificação ##
  filter(atraso >= 0) %>% ## retirando atrasos menores que 0
  as.data.frame()

diadema_obitos<-diadema %>% 
  filter(!is.na(data_pri_sin) & ## retirando datas de 1º sintomas com NA
           !is.na(data_notificacao) & ## retirando datas de notificação com NA
           !is.na(obito)) %>% ## Selecionando quem É óbito 
  mutate(data_da_coleta = as.Date(data_da_coleta, "%d/%m/%Y"), ## Transformando em formato data padronizado
         data_pri_sin = as.Date(data_pri_sin, "%d/%m/%Y"),
         data_do_obito = as.Date(data_do_obito, "%d/%m/%Y"),
         data_notificacao = as.Date(data_notificacao, "%d/%m/%Y"),
         atraso = as.numeric(data_notificacao - data_pri_sin)) %>% ## calculando o atraso entre 1º sintomas e notificação ##
  filter(atraso >= 0) %>% ## retirando atrasos menores que 0
  as.data.frame()

## Somando os casos e criando uma série temporal por data de 1º sintomas
diadema_sin_pri<-diadema_casos %>% 
  group_by(data_pri_sin) %>% 
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

## Somando os casos e criando uma série temporal por data de notificação 
diadema_not<-diadema_casos %>% 
  group_by(data_notificacao) %>% 
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

## plotando os casos por ambas as datas, 1º sintomas e notificação
p.plot<-ggplot(data = diadema_sin_pri, aes(x=data_pri_sin, y = N))+
  geom_line(col = "red")+
  geom_line(data = diadema_not, aes(x=data_notificacao, y=N), col = "blue")+
  xlab("Datas") +
  ylab("Nº Casos") +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  ggtitle("Diadema - SP")
p.plot

## Criando do data.frame com as datas que serão entregues ao nowcasting, importante, 
## não conter atrasos negativos, e estarem em formato data
diadema_datas<-diadema_casos %>% 
  filter(!is.na(data_pri_sin) & ## reitrando datas de 1º sintomas com NA, caso ainda haja alguma
           !is.na(data_notificacao) & ## reitrando datas de notificação com NA, caso ainda haja alguma
           atraso >= 0) %>% ## reitrando atrasos negativos, caso ainda haja algum
  mutate(data_pri_sin = as.Date(data_pri_sin, format = "%Y-%m-%d"),
         data_notificacao = as.Date(data_notificacao, format = "%Y-%m-%d")) %>% 
  select(data_pri_sin, data_notificacao) %>% ## Selecionando somente as colunas de datas para serem entregues ao NobBS
  as.data.frame()

## Salvando o CSV, para rodadas futuras
# write.csv(diadema_datas, file = "./scripts_R_genericos/Diadema/diadema_datas.csv", row.names = FALSE)
# diadema_datas<-read_csv("./scripts_R_genericos/Diadema/diadema_datas.csv")

## Nowcasting 
nowcasting_diadema<-NobBS(data = diadema_datas, ## Data.frame com colunas de datas de 1º sintomas e notificação somente
                          now = max(diadema_datas$data_pri_sin), ## até aonde fazer o nowcasting, só podemos fazer até ultima data de 1º sintomas
                          onset_date = "data_pri_sin", ## Coluna com as datas dos eventos, onset, nosso caso aqui são os 1º sintomas
                          report_date = "data_notificacao", ## Coluna com as datas da notificaçaõ do evento, report, nosso caso aqui são notificação mesmo
                          units = "1 day", ## unidade de dias em que o nowcasting deve operar 
                          moving_window = 60, ## IMPORTANTE! Janela em o nowcasting deve operar, como na nossa série há casos com atraso entre 1º sintomas e notificaçõa muito distante, temos que fixar a área de atuação
                          specs = list(nAdapt = 5000, nBurnin = 3000, nThin = 1, nSamp = 10000)) ## parâmetros pro nowcasting
betas_diadema<-beta.summary(nowcasting_diadema) #### função em funcoes.R, funçaõ que organiza os atrasos, função que desenvolvemos, deve vir no load lá de cima

## Salvando as estimativas do nowcasting ##
write.csv(nowcasting_diadema$estimates, 
          file = "nowcasting_diadema.csv", 
          row.names = FALSE)
## Salvando os betas, atrasos entre 1º sintomas e notificação ##
write.csv(betas_diadema, 
          file = "betas_diadema.csv",
          row.names = FALSE)

## Gráficos ##

## Atrasos ##
p.betas.diadema<-
  ggplot(betas_diadema, aes(atraso, mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  xlab("Dias após 1º sintomas") +
  ylab("Probabilidade de notificação") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Atraso de notificação Diários")
p.betas.diadema

## Salvando a figura ##
ggsave(p.betas.diadema, filename = paste0("betas_diadema.png"),
       dpi = 600, width = 9, height = 7)

## Nowcasting ##
p.prev.ic_diadema<-nowcasting_diadema$estimates %>%
  ggplot(aes(x = onset_date, y = estimate)) +
  geom_line(data = diadema_sin_pri, aes(x = data_pri_sin, y = N, color="Notificados"), lwd = 0.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Data 1º sintomas") +
  ylab("Nº de Casos por dia") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "dark orange"), aesthetics = c("colour", "fill"))+
  ggtitle("Casos Diários")
p.prev.ic_diadema

## Salvando a figura ##
ggsave(p.prev.ic_diadema, filename = paste0("nowcasting_diadema.png"),
       dpi = 600, width = 9, height = 7)


## filtrando e dando zoom no fim da curva ##
diadema_sin_pri2<-diadema_sin_pri %>% 
  filter(data_pri_sin >= min(nowcasting_diadema$estimates$onset_date)) %>% 
  as.data.frame()

## Nowcasting somente para aonde o nowcasting atuou ##
p.prev.ic_diadema2<-nowcasting_diadema$estimates %>%
  ggplot(aes(x = onset_date, y = estimate)) +
  geom_line(data = diadema_sin_pri2, aes(x = data_pri_sin, y = N, color="Notificados"), lwd = 0.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Data 1º sintomas") +
  ylab("Nº de Casos por dia") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("red", "dark orange"), aesthetics = c("colour", "fill"))+
  ggtitle("Casos Diários")
p.prev.ic_diadema2

## Salvando a figura ##
ggsave(p.prev.ic_diadema2, filename = paste0("nowcasting_diadema_zoom.png"),
       dpi = 600, width = 9, height = 7)

## Painel com ambas as figuras ##
arrange<-ggarrange(p.prev.ic_diadema, p.prev.ic_diadema2)
arrange

## Salvando o painel ##
ggsave(arrange, filename = paste0("arrange_diadema_zoom.png"),
       dpi = 600, width = 9, height = 7)

## Nowcasting por área de abrangência ##

## Criando um data.frame com datas e área de abrangêngia ##
diadema_datas_strat<-diadema_casos %>% 
  filter(!is.na(data_pri_sin) & ## retirando datas de 1º sintomas com NA
           !is.na(data_notificacao) & ## retirando datas de notificação com NA
           atraso >= 0 & ## retirando atrasos negativos
           agrega_aa != "IGN") %>% ## Agrega com preenchimento IGN
  mutate(data_pri_sin = as.Date(data_pri_sin, format = "%Y-%m-%d"), ## trasnformando em datas 
         data_notificacao = as.Date(data_notificacao, format = "%Y-%m-%d")) %>% ## trasnformando em datas 
  select(data_pri_sin, 
         data_notificacao, 
         agrega_aa) %>% ## selecionando as colunas a serem usadas
  as.data.frame()

## criando a série temporal para cada área de abrangência ##
diadema_pri_sin_stat<-diadema_datas_strat %>% 
  group_by(data_pri_sin, agrega_aa) %>% 
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N),
         stratum  = agrega_aa)%>% ## modificando o nome para ficar no padrão que o nowcasting devolverá
  as.data.frame()

## Nowcasting com estratificação por área de abrangência ##
nowcasting_diadema_stratum<-NobBS.strat(data = diadema_datas_strat, ##Veja que é uma outra função, mas como mesmo parâmetros
                          now = max(diadema_datas$data_pri_sin), ## definindo Now
                          onset_date = "data_pri_sin", ## indicando coluna de onset
                          report_date = "data_notificacao", ## indicando coluna de report
                          units = "1 day", ## unidade de tempo
                          moving_window = 60, ## janela de atuação 
                          strata = "agrega_aa", ## novo parâmetro, stratum, ou estratificação, indicando a coluna para área de abrangência
                          specs = list(nAdapt = 5000, nBurnin = 3000, nThin = 1, nSamp = 10000)) ## parâmetros pro nowcasting 

## filtrando as datas em que o nowcasting atuou somente 
diadema_pri_sin_stat<-diadema_pri_sin_stat %>% 
  filter(data_pri_sin >= min(nowcasting_diadema$estimates$onset_date)) %>% 
  as.data.frame()

## plotando painel do nowcasting para cada área de abrangência ##
p.prev.ic_diadema3<-nowcasting_diadema_stratum$estimates %>%
  ggplot(aes(x = onset_date, y = estimate)) +
  geom_line(data = diadema_pri_sin_stat, aes(x = data_pri_sin, y = N, color="Notificados"), lwd = 0.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Data 1º sintomas") +
  ylab("Nº de Casos por dia") +
  theme_bw() +
  theme(legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "dark orange"), aesthetics = c("colour", "fill"))+
  ggtitle("Casos Diários")+
  facet_wrap(~stratum, scales = "free")
p.prev.ic_diadema3

## Salvando painel ##
ggsave(p.prev.ic_diadema3, filename = paste0("nowcasting_diadema_stratum.png"),
       dpi = 600, width = 9, height = 7)
