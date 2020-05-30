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

PRJROOT  = rprojroot::find_root(".here")

source("./funcoes.R")
source("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/_src/funcoes.R")

data_ultimo_boletim<-as.Date("2020-05-08")

uol<-read_csv("./analise UOL/SRAGs-tabela-last-updated_revised2.csv")
uol<-as.data.frame(uol)
uol$Data<-as.Date(uol$Data, format = "%d/%m/%Y")
uol_melted<-reshape::melt(uol, id.vars = "Data")

p.uol <-
  ggplot(uol_melted, aes(x = Data, y = value, col = variable)) +
  geom_point(shape = 1)+
  geom_line()+
  geom_vline(xintercept = as.Date("2020-03-17", format = "%Y-%m-%d"), colour = "indianred3", size = 0.45, linetype = "dashed")+
  scale_color_viridis_d(name = "Data Boletim", option = "Greens", direction = 1)+
  labs(x = "Data", y = "Número de Óbitos") +
  theme_bw() +
  theme(legend.position = "right")+
  theme(axis.text= element_text(size=14),
        axis.title = element_text(size=14))
p.uol
ggsave(plot = p.uol, filename = "./analise UOL/plots/curvas_boletins_08_05.png", dpi = 600, width = 9, height = 7)


######SALVANDO EM SVG######
plots.para.atualizar<-p.uol
filepath<-"./analise UOL/plots/curvas_boletins_08_05"

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

##########################
##      NOWCASTING      ##
##########################
uol <- read.csv("./analise UOL/SRAGs-tabela-last-updated_revised2.csv") ##load da CSV Utilizada
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
## Loop que vai preenchendo a nova matriz com a difreença da coluna mais a direita pra seguinte
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
datas.boletins <- as.character(as.Date(names(uol3.df), "X%d.%m.%y"))
datas.obito <- as.character(as.Date(as.character(uol4$data), "%d/%m/%y"))
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
         Report_date = as.Date(uol_df$uol_report_date, format = "%d/%m/%Y")) %>%
  as.data.frame()
### somando os óbitos ###
uol_df2 = uol_df %>%
  group_by(Death_date)%>%
  dplyr::summarise(N=n())%>%
  as.data.frame()

###Nowcasting ###
nowcasting6<- NobBS(data = uol_df,
                    now = max(uol_df$Death_date),
                    onset_date = "Death_date",
                    report_date = "Report_date",
                    units = "1 day",
                    specs = list(nAdapt = 3000, nBurnin = 3000, nThin = 1, nSamp = 10000)
)
betas6<-beta.summary(nowcasting6)

nowcasting6.30<- NobBS(data = uol_df,
                    now = max(uol_df$Death_date),
                    onset_date = "Death_date",
                    report_date = "Report_date",
                    units = "1 day",
                    moving_window = 30,
                    specs = list(nAdapt = 3000, nBurnin = 3000, nThin = 1, nSamp = 10000)
)
betas6.30<-beta.summary(nowcasting6.30)

nowcasting6.20<- NobBS(data = uol_df,
                       now = max(uol_df$Death_date),
                       onset_date = "Death_date",
                       report_date = "Report_date",
                       units = "1 day",
                       moving_window = 20,
                       specs = list(nAdapt = 3000, nBurnin = 3000, nThin = 1, nSamp = 10000)
)
betas6.20<-beta.summary(nowcasting6.20)

nowcasting6.10<- NobBS(data = uol_df,
                       now = max(uol_df$Death_date),
                       onset_date = "Death_date",
                       report_date = "Report_date",
                       units = "1 day",
                       moving_window = 10,
                       specs = list(nAdapt = 3000, nBurnin = 3000, nThin = 1, nSamp = 10000)
)
betas6.10<-beta.summary(nowcasting6.10)


####Nowcasting pro penultimo dia####
nowcasting7<- NobBS(data = uol_df,
                    now = max(uol_df$Death_date)-1,
                    onset_date = "Death_date",
                    report_date = "Report_date",
                    units = "1 day",
                    specs = list(nAdapt = 3000, nBurnin = 3000, nThin = 1, nSamp = 10000)
)
betas7<-beta.summary(nowcasting7)

####Nowcasting pro antepenultimo dia####
nowcasting8<- NobBS(data = uol_df,
                    now = max(uol_df$Death_date)-2,
                    onset_date = "Death_date",
                    report_date = "Report_date",
                    units = "1 day",
                    specs = list(nAdapt = 3000, nBurnin = 3000, nThin = 1, nSamp = 10000)
)
betas8<-beta.summary(nowcasting8)

################################################################################
## Plots: objetos ggplot2
################################################################################
## Tempos de atraso ##
p.betas <-
  ggplot(betas6, aes(atraso, mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  xlab("Dias após dia do óbito") +
  ylab("Probabilidade de notificação") +
  theme_bw() +
  theme(legend.position="none") 
  # ggtitle("Atraso de notificação de óbitos COVID por SRAG")
p.betas
p.betas %+% betas6.10
p.betas %+% betas6.20
p.betas %+% betas6.30
p.betas %+% betas7 ###para até o penultimo dia###
p.betas %+% betas8 ###para até o antepenultimo dia###
# ggsave(plot = p.betas, filename = "plots/betas_29_04.png", dpi = 600, width = 9, height = 7)

## N de casos previstos e seus ICS ##
p.prev.ic <- ggplot(nowcasting6$estimates, aes(x = onset_date, y = estimate)) +
  geom_line(data = uol_df2, aes(x = Death_date, y = N),
            col = "skyblue", lwd = 1.2) +
  geom_line(aes(x = onset_date, y = n.reported), col = "blue", lwd = 1.2)+
  geom_line(col = "indianred3") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha =0.15, fill = "indianred3") +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Nowcasting de óbitos de COVID-19")
p.prev.ic
p.prev.ic %+% nowcasting6.10$estimates
p.prev.ic %+% nowcasting6.20$estimates
p.prev.ic %+% nowcasting6.30$estimates
p.prev.ic %+% nowcasting7$estimates
p.prev.ic %+% nowcasting8$estimates
# ggsave(plot = p.prev.ic, filename = "./analise UOL/plots/nowcasting_08_05.png", dpi = 600, width = 9, height = 7)

###Comparando nowcasting###
p.nows<-ggplot(nowcasting6$estimates, aes(x = onset_date, y = estimate)) +
  # geom_line(data = nowcasting6$estimates, aes(x = onset_date, y = n.reported), 
            # col = "skyblue", lwd = 1.2) +
  geom_line(data = uol_df2, aes(x = Death_date, y = N),
            col = "skyblue", lwd = 1.2) +
  geom_line(col = "indianred3") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha =0.15, fill = "indianred3") +
  # geom_line(data = nowcasting7$estimates, aes(x = onset_date, y = estimate), col = "darkgreen") +
  # geom_ribbon(data = nowcasting7$estimates,
              # aes(ymin = lower, ymax = upper), alpha =0.15, fill = "darkgreen") +
  # geom_line(data = nowcasting8$estimates, aes(x = onset_date, y = estimate), col = "purple") +
  # geom_ribbon(data = nowcasting8$estimates,
              # aes(ymin = lower, ymax = upper), alpha =0.15, fill = "purple") +
  geom_line(data = nowcasting6.10$estimates, aes(x = onset_date, y = estimate), col = "purple") +
  geom_ribbon(data = nowcasting6.10$estimates,
              aes(ymin = lower, ymax = upper), alpha =0.15, fill = "purple") +
  geom_line(data = nowcasting6.20$estimates, aes(x = onset_date, y = estimate), col = "dark orange") +
  geom_ribbon(data = nowcasting6.20$estimates,
              aes(ymin = lower, ymax = upper), alpha =0.15, fill = "dark orange") +
  geom_line(data = nowcasting6.30$estimates, aes(x = onset_date, y = estimate), col = "darkgreen") +
  geom_ribbon(data = nowcasting6.30$estimates,
              aes(ymin = lower, ymax = upper), alpha =0.15, fill = "darkgreen") +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Nowcasting de óbitos de COVID-19")
p.nows
# ggsave(plot = p.nows, filename = "./analise UOL/plots/compara_nowcasting_07_05.png", dpi = 600, width = 9, height = 7)

#############################
######### Gráficos ##########
#############################
uol_final<-nowcasting6$estimates
uol_final<-uol_final[, c("estimate", "lower", "upper")]
uol_final2<-as.data.frame(cbind(uol_final, 
                                "Data" = as.Date(uol_df2$Death_date, "%Y-%m-%d"), 
                                "Boletim ultimo" = uol[,2]))

uol_final3<-apply(t(uol_final2[,-4]), 1, cumsum)
colnames(uol_final3)<-c("estimate Cumsum", "lower Cumsum", "upper Cumsum", "Boletim Cumsum")

uol_final4<-as.data.frame(cbind("Data" = uol_final2$Data, uol_final2[,-4], uol_final3))
# write.csv(uol_final4, file = "./analise UOL/spreasheet e CSV/uol_final_nowcasting_08_05_cumsum_30.csv", row.names = FALSE)
# uol_final4<-read_csv("./analise UOL/spreasheet e CSV/uol_final_nowcasting_08_05_cumsum_sem_janela.csv")
# uol_final5<-read_csv("./analise UOL/spreasheet e CSV/uol_final_nowcasting_08_05_cumsum_10.csv")
# uol_final6<-read_csv("./analise UOL/spreasheet e CSV/uol_final_nowcasting_08_05_cumsum_20.csv")
# uol_final7<-read_csv("./analise UOL/spreasheet e CSV/uol_final_nowcasting_08_05_cumsum_30.csv")

p.prev.ic2 <- ggplot(uol_final4, aes(x = Data, y = `estimate`)) +
  geom_line(data = uol_final4, aes(x = Data, y = `Boletim ultimo`, color="Notificados"), lwd = 1.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =`lower`, ymax = `upper`), fill="red", alpha =0.15) +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos por dia") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill"))
  # ggtitle("Nowcasting de óbitos de COVID-19 anunciados pelo MS para o  Brasil, até 08 de maio")
p.prev.ic2
# ggsave(plot = p.prev.ic2, filename = "./analise UOL/plots/nowcasting_08_05.png", dpi = 600, width = 9, height = 7)

p.prev.ic.cumsum <- ggplot(uol_final4, aes(x = Data, y = `estimate Cumsum`)) +
    geom_line(data = uol_final4, aes(x = Data, y = `Boletim Cumsum`, color="Notificados"), lwd = 1.5) +
    geom_line(aes(col = "Estimado")) +
    geom_ribbon(aes(ymin =`lower Cumsum`, ymax = `upper Cumsum`), fill="red", alpha =0.15) +
    # geom_line(data = uol_final5, aes(x = Data, y = `estimate Cumsum`), color = "darkgreen")+
    # geom_ribbon(data = uol_final5 , aes(ymin =`lower Cumsum`, ymax = `upper Cumsum`), fill="dark green", alpha =0.15) +
    # geom_line(data = uol_final6, aes(x = Data, y = `estimate Cumsum`), color = "purple")+
    # geom_ribbon(data = uol_final6 , aes(ymin =`lower Cumsum`, ymax = `upper Cumsum`), fill="purple", alpha =0.15) +
    # geom_line(data = uol_final7, aes(x = Data, y = `estimate Cumsum`), color = "dark orange")+
    # geom_ribbon(data = uol_final7 , aes(ymin =`lower Cumsum`, ymax = `upper Cumsum`), fill="dark orange", alpha =0.15) +
    xlab("Dia do Óbito") +
    ylab("Nº de Óbitos Acumulados") +
    theme_bw() +
    theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
    scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill")) 
    # ggtitle("Nowcasting de óbitos de COVID-19 anunciados pelo MS para o Brasil, até 08 de maio")
p.prev.ic.cumsum
# ggsave(plot = p.prev.ic.cumsum, filename = "./analise UOL/plots/nowcasting_08_05_cumsum.png", dpi = 600, width = 9, height = 7)

uol_final4<-uol_final4[,-c(2:5)]
write.csv(uol_final4, file = "./analise UOL/spreasheet e CSV/uol_final_nowcasting_08_05.csv", row.names = FALSE)


######PLOT CUMSUM BETAS##########
betas6_cumsum<-apply(t(betas6[,-1]), 1, cumsum)
betas6_cumsum<-as.data.frame(cbind(betas6_cumsum, Atraso = betas6$atraso))

p.betas.cumsum <- ggplot(betas6_cumsum, aes(x = Atraso, y = mean)) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Dias de até a Notificação") +
  ylab("Porcentagem de óbitos já notificados") +
  theme_bw() +
  # theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  theme(legend.position = "none")+
  scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill")) +
  ggtitle("Notificação Acumulativa de óbitos")
p.betas.cumsum


###########################
######SALVANDO EM SVG######
###########################
p.betas
# p.betas.cumsum

p.prev.ic.cumsum
p.prev.ic2

plots.para.atualizar<-list(p.prev.ic2, p.prev.ic.cumsum)
plots.para.atualizar<-p.prev.ic.cumsum
filepath<-"./analise UOL/plots/plots SVG/nowcasting_BE_MS_08_maio_cumsum"

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


