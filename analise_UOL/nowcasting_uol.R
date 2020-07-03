###Análise UOL###
#################
if(!require(plyr)){install.packages("plyr"); library(plyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(NobBS)){install.packages("NobBS"); library(NobBS)}
if(!require(aweek)){install.packages("aweek"); library(aweek)}
if(!require(cowplot)){install.packages("cowplot"); library(cowplot)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
# if(!require(brms)){install.packages("brms"); library(brms)}
if(!require(rprojroot)){install.packages("rprojroot"); library(rprojroot)}
if(!require(zoo)){install.packages("zoo"); library(zoo)}
if(!require(EpiEstim)){install.packages("EpiEstim"); library(EpiEstim)}
if(!require(foreign)){install.packages("foreign"); library(foreign)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}
if(!require(aweek)){install.packages("aweek"); library(aweek)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}

PRJROOT  = rprojroot::find_root(".here")

devtools::load_all("./now_fcts/R/") ##loading de funções necessárias##
  
uol<-read_csv("./analise_UOL/dados/SRAGs-tabela-last-updated.csv")
###SEGUIR IGUAL###

uol<-as.data.frame(uol)
uol$Data<-as.Date(uol$Data, format = "%d/%m/%Y")
uol_melted<-reshape::melt(uol, id.vars = "Data")

p.uol <-
  ggplot(uol_melted, aes(x = Data, y = value, col = variable)) +
  geom_point(shape = 1)+
  geom_line()+
  geom_vline(xintercept = as.Date("2020-03-17", format = "%Y-%m-%d"), colour = "indianred3", size = 0.45, linetype = "dashed")+
  scale_color_viridis_d(name = "Data Boletim", 
                        option = "plasma", 
                        direction = 1, 
                        alpha = 1, 
                        # begin = 0, 
                        end = 1)+
  labs(x = "Data", y = "Número de Óbitos") +
  theme_bw() +
  theme(legend.position = "right")+
  theme(axis.text= element_text(size=14),
        axis.title = element_text(size=14))
p.uol

##########################
##      NOWCASTING      ##
##########################
uol<-read_csv("./analise_UOL/dados/SRAGs-tabela-last-updated.csv")
uol<-as.data.frame(uol)
uol2<-uol #variavel auxiliar
uol2[is.na(uol2)] <- 0 ##preenchendo com 0 onde não há dados ###
uol2.1<-uol2 ###variavel auxiliar
###loop pra contar os valores do boletins mais antigo e contabilizar esse valor nos boletins mais recentes###
###para manter a data de divulgação como a data do boletim em que a morte é contada pela primeira vez #######
for(i in 3:length(uol2)){
  for(k in 1:dim(uol2)[1]){
    if(uol2.1[k,i]>uol2.1[k,(i-1)] & uol2.1[k,i]>0){
      uol2.1[k,i]=uol2.1[k,(i-1)]
    }
  }
}
uol2<-uol2.1 ###devolvendo o resultado na variável utilizada ###
## matriz para guardar os resultados
uol3 <- matrix( nrow=nrow(uol2), ncol=ncol(uol2)-1)
## Coloca a ultima coluna dos dados na ultima coluna da matriz
uol3[,ncol(uol3)] <- uol2[,ncol(uol2)]
## Loop que vai preenchendo a nova matriz com a diferença da coluna mais a direita pra seguinte
for(i in ncol(uol2):3){
  uol3[,(i-2)] <- uol2[,(i-1)] - uol2[,i]
}
## Conferindo
all(apply(uol3, 1, sum) == uol2[,2]) ## ok!
## Junta com coluna de datas dos boletins e nomeia as colunas
uol3.df <- as.data.frame(uol3)
names(uol3.df) <- names(uol2)[-1]
uol4 <- cbind(data=uol2[,1], uol3.df)
## Monta data.frame de datas de obito e de regitro
datas.boletins <- as.character(as.Date(names(uol3.df), "%d/%m/%y"))
datas.obito <- as.character(as.Date(uol4$data, "%d/%m/%y"))
## Vetores para guardar as datas de evento e 
onset.dates <- record.dates <- c()
## Loop sobre as linhas e então colunas da matriz
for(i in 1:length(datas.obito)){
  for(j in 1:length(datas.boletins)){
    if(uol3[i,j]>0){
      onset.dates <- c(onset.dates, rep(datas.obito[i], uol3[i,j]))
      record.dates <- c(record.dates, rep(datas.boletins[j], uol3[i,j]))
    }
  }
}

## Conferindo o total de óbitos ##
length(record.dates)
length(onset.dates)
sum(uol2[,2])

## Monta o data.frame
uol_df <- data.frame(uol_death_date = as.Date(onset.dates), uol_report_date = as.Date(record.dates)) 
## Transformando em datas que entram no nowcasting##
uol_df = uol_df %>%
  mutate(Death_date = as.Date(uol_df$uol_death_date, format = "%d/%m/%Y"), 
         Report_date = as.Date(uol_df$uol_report_date, format = "%d/%m/%Y"),
         Death_week = date2week(Death_date, start = "sun", floor_day = TRUE),
         Report_week = date2week(Report_date, start = "sun", floor_day = TRUE),
         Death_wd = week2date(Death_week, floor_day = TRUE),
         Report_wd = week2date(Report_date, floor_day = TRUE)
         ) %>%
  as.data.frame()
### somando os óbitos ###
uol_df2 = uol_df %>%
  group_by(Death_date)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

### somando os óbitos ###
uol_df3 = uol_df %>%
  group_by(Death_wd)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

trim<-0 ## cortando o fim da sequência de datas para evitar ruídos demais ##

## Daily ##
nowcasting<-NobBS.posterior2(data = uol_df,
                                 now = max(uol_df$Death_date)-trim,
                                 onset_date = "Death_date",
                                 report_date = "Report_date",
                                 units = "1 day",
                                 specs = list(nAdapt = 8000, nBurnin = 3000, nThin = 1, nSamp = 10000))
betas<-beta.summary(nowcasting) #### função em funcoes.R`
betas_cumsum<-beta.cumsum(nowcasting, samples = 5000)
nowcasting_cumsum<-nowcasting.cumsum(nowcasting, samples = 5000)

## Weekly ##
nowcasting_w<-NobBS.posterior2(data = uol_df,
                             now = max(uol_df$Death_wd),
                             onset_date = "Death_wd",
                             report_date = "Report_wd",
                             units = "1 week",
                             specs = list(nAdapt = 7000, nBurnin = 3000, nThin = 1, nSamp = 10000))
betas_w<-beta.summary(nowcasting_w) #### função em funcoes.R`
betas_cumsum_w<-beta.cumsum(nowcasting_w, samples = 5000)
nowcasting_cumsum_w<-nowcasting.cumsum(nowcasting_w, samples = 5000)

#############################
######### Gráficos ##########
#############################

################################################################################
## Plots: objetos ggplot2
################################################################################

## Tempos de atraso ##
p.betas <-
  ggplot(betas, aes(atraso, mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  xlab("Dias após dia do óbito") +
  ylab("Probabilidade de notificação") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Atraso de notificação de óbitos COVID por SRAG")
p.betas

p.betas_w<-p.betas %+% betas_w +
  xlab("Semanas após dia do óbito")
p.betas_w

## Tempos de atraso cumulativo ##
p.betas_cumsum <- p.betas %+% betas_cumsum +
  ylab("Probabilidade Acumulada\n de notificação")
p.betas_cumsum

p.betas_cumsum_w <- p.betas_cumsum %+% betas_cumsum_w +
  xlab("Semanas após dia do óbito")
p.betas_cumsum_w

## N de casos previstos e seus ICS ##
p.prev.ic <- nowcasting$estimates %>% ggplot(aes(x = onset_date, y = estimate)) +
  geom_line(data = uol_df2, aes(x = Death_date, y = N, color="Notificados"), lwd = 1.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos por dia") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill"))+
  ggtitle("Diários")
p.prev.ic

p.prev.ic_w<-nowcasting_w$estimates %>% ggplot(aes(x = onset_date, y = estimate)) +
  geom_line(data = uol_df3, aes(x = Death_wd, y = N, color="Notificados"), lwd = 1.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Semana do Óbito") +
  ylab("Nº de Óbitos por semana") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill"))+
  ggtitle("Semanal")
p.prev.ic_w

p.prev.ic.cumsum<-ggplot(nowcasting_cumsum, aes(x= Dates, y = mean))+
  geom_line(data = uol_df2, aes(x = Death_date, y = Cum, color="Notificados"), lwd = 1.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos Acumulados") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill"))+
  ggtitle("Acumulados Diários")
p.prev.ic.cumsum

p.prev.ic.cumsum_w<-ggplot(nowcasting_cumsum_w, aes(x= Dates, y = mean))+
  geom_line(data = uol_df3, aes(x = Death_wd, y = Cum, color="Notificados"), lwd = 1.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Semana do Óbito") +
  ylab("Nº de Óbitos Acumulados") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill"))+
  ggtitle("Acumulados Semanal")
p.prev.ic.cumsum_w

p.arrange<-ggpubr::ggarrange(p.betas, p.betas_cumsum, p.prev.ic, p.prev.ic.cumsum)
p.arrange
p.arrange_w<-ggpubr::ggarrange(p.betas_w, p.betas_cumsum_w, p.prev.ic_w, p.prev.ic.cumsum_w)
p.arrange_w
ggsave(p.arrange, filename = "./analise_UOL/plots/arrange_nowcasting_13_06.png", 
       dpi = 600, width = 9, height = 7)

###########################
######SALVANDO EM SVG######
########################### 
# plots.para.atualizar<-p.prev.ic
# filepath<-"./analise UOL/plots/plots SVG/sensibilidade_nowcasting_BE_MS_08_maio"
plots.para.atualizar<-p.prev.ic.cumsum
filepath<-"./analise_UOL/plots/plots_SVG/nowcasting_BE_MS_05_junho_cumsum"

graph.svg <- plots.para.atualizar + theme(axis.text=element_text(size=6.65), # corrige a diferenca do tamanho do texto entre svg e html
                                          plot.margin = margin(10, 0, 0, 7, "pt")) # corrige a margem inserida pelo plotly
ggsave(paste(filepath,".svg",sep=""), plot = graph.svg, device = svg, scale = 1, width = 215, height = 146, units = "mm")
# tamanho calculado usando ppi = 141.21
# o tamanho do texto no placeholder deve ser um fator de 0.665 do tamanho original
# large
graph.sm.svg <- graph.svg + theme(axis.text=element_text(size=8.65)) # corrige a diferenca do tamanho do texto entre svg e html
ggsave(paste(filepath,".lg.svg",sep=""), plot = graph.sm.svg, device = svg, scale = 1, width = 215, height = 146, units = "mm")
# medium
graph.sm.svg <- graph.svg + theme(axis.text=element_text(size=12.65)) # corrige a diferenca do tamanho do texto entre svg e html
ggsave(paste(filepath,".md.svg",sep=""), plot = graph.sm.svg, device = svg, scale = 1, width = 215, height = 146, units = "mm")
# small
graph.sm.svg <- graph.svg + theme(axis.text=element_text(size=16.65)) # corrige a diferenca do tamanho do texto entre svg e html
ggsave(paste(filepath,".sm.svg",sep=""), plot = graph.sm.svg, device = svg, scale = 1, width = 215, height = 146, units = "mm")
# extra small
graph.sm.svg <- graph.svg + theme(axis.text=element_text(size=20.65)) # corrige a diferenca do tamanho do texto entre svg e html
ggsave(paste(filepath,".ex.svg",sep=""), plot = graph.sm.svg, device = svg, scale = 1, width = 215, height = 146, units = "mm")
