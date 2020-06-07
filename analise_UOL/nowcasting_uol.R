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

source("./site/_src/fct/funcoes.R")

data_ultimo_boletim<-as.Date("2020-06-05")

# uol<-read_csv("./analise_UOL/SRAGs-tabela-last-updated_revised-29_maio.csv") ### load da CSV, essa é sem preenchimento dos BE faltantes
uol<-read_csv("./analise_UOL/SRAGs-tabela-last-updated_revised3.csv")
###SEGUIR IGUAL###

uol<-as.data.frame(uol)
uol$Data<-as.Date(uol$Data, format = "%d/%m/%Y")
uol_melted<-reshape::melt(uol, id.vars = "Data")

p.uol <-
  ggplot(uol_melted, aes(x = Data, y = value, col = variable)) +
  geom_point(shape = 1)+
  geom_line()+
  geom_vline(xintercept = as.Date("2020-03-17", format = "%Y-%m-%d"), colour = "indianred3", size = 0.45, linetype = "dashed")+
  scale_color_viridis_d(name = "Data Boletim", option = "cividis", direction = 1)+
  labs(x = "Data", y = "Número de Óbitos") +
  theme_bw() +
  theme(legend.position = "right")+
  theme(axis.text= element_text(size=14),
        axis.title = element_text(size=14))
p.uol
#   
# uol2<-as.matrix(uol[,-1]) #variavel auxiliar
# rownames(uol2)<-uol$Data
# uol2<-normalize.rows(uol)
# 
# uol$Data<-as.Date(uol$Data, format = "%d/%m/%Y")
# uol_melted<-reshape::melt(uol, id.vars = "Data")
# 
# 
# p.uol.ridges <- ggplot(uol_melted, aes(x = Data, y = variable)) +
#   geom_joy(aes(colour = variable)) +
#   theme_ridges()+
#   xlab("Data de óbito")+
#   ylab("Boletins")+
#   ggtitle("Boletins Epidemiológicos - MS")
# p.uol.ridges
# 
# ggplot(uol_melted, aes(x = Data, y = as.factor(variable), fill = ..x..)) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   scale_fill_viridis(name = "Óbitos por dia", option = "C") +
#   labs(title = 'Boletins Epidemiológicos Ministério da Saúde') +
#   theme_ridges() +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   )

##########################
##      NOWCASTING      ##
##########################
# uol<-read_csv("./analise_UOL/SRAGs-tabela-last-updated_revised-29_maio.csv") ### load da CSV, essa é sem preenchimento dos BE faltantes
uol<-read_csv("./analise_UOL/SRAGs-tabela-last-updated_revised3.csv") ##load da CSV Utilizada
# uol<-as.data.frame(cbind(uol$Data, round(uol[,-1]))) ###Somente pra que tem preenchimento, pra arredondar os números###
uol<-as.data.frame(uol)
# uol$Data<-as.Date(uol$Data, format = "%d/%m/%Y")
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
         Report_date = as.Date(uol_df$uol_report_date, format = "%d/%m/%Y")) %>%
  as.data.frame()
### somando os óbitos ###
uol_df2 = uol_df %>%
  group_by(Death_date)%>%
  dplyr::summarise(N=n())%>%
  as.data.frame()

###Nowcasting ###
nowcasting<- NobBS(data = uol_df,
                    now = max(uol_df$Death_date),
                    onset_date = "Death_date",
                    report_date = "Report_date",
                    units = "1 day",
                    specs = list(nAdapt = 3000, nBurnin = 3000, nThin = 1, nSamp = 10000)
)
betas<-beta.summary(nowcasting) #### função em funcoes.R
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

## N de casos previstos e seus ICS ##
p.prev.ic <- ggplot(nowcasting$estimates, aes(x = onset_date, y = estimate)) +
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


#############################
######### Gráficos ##########
#############################
uol_final<-nowcasting$estimates
uol_final<-uol_final[, c("estimate", "lower", "upper")]
uol_final2<-as.data.frame(cbind(uol_final, 
                                "Data" = as.Date(uol_df2$Death_date, "%Y-%m-%d"), 
                                "Boletim ultimo" = uol[,2]))
write.csv(uol_final2, file = "./analise_UOL/spreasheet_e_CSV/uol_final_nowcasting_03_06.csv", row.names = FALSE)
uol_final3<-apply(t(uol_final2[,-4]), 1, cumsum)
colnames(uol_final3)<-c("estimate Cumsum", "lower Cumsum", "upper Cumsum", "Boletim Cumsum")

uol_final4<-as.data.frame(cbind("Data" = uol_final2$Data, uol_final2[,-4], uol_final3))
write.csv(uol_final4, file = "./analise_UOL/spreasheet_e_CSV/uol_final_nowcasting_03_06_cumsum.csv", row.names = FALSE)


p.prev.ic2 <- ggplot(uol_final4, aes(x = Data, y = `estimate`)) +
  geom_line(data = uol_final4, aes(x = Data, y = `Boletim ultimo`, color="Notificados"), lwd = 1.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =`lower`, ymax = `upper`), fill="red", alpha =0.15) +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos por dia") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill"))+
  ggtitle("Diários")
p.prev.ic2

p.prev.ic.cumsum <- ggplot(uol_final4, aes(x = Data, y = `estimate Cumsum`)) +
    geom_line(data = uol_final4, aes(x = Data, y = `Boletim Cumsum`, color="Notificados"), lwd = 1.5) +
    geom_line(aes(col = "Estimado")) +
    geom_ribbon(aes(ymin =`lower Cumsum`, ymax = `upper Cumsum`), fill="red", alpha =0.15) +
      xlab("Dia do Óbito") +
    ylab("Nº de Óbitos Acumulados") +
    theme_bw() +
    theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
    scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill"))+
    ggtitle("Acumulados")
p.prev.ic.cumsum

p.arrange<-ggpubr::ggarrange(p.prev.ic2, p.prev.ic.cumsum)
p.arrange
ggsave(p.arrange, filename = "./analise_UOL/plots/arrange_nowcasting_03_06.png", 
       dpi = 600, width = 9, height = 7)

p.annotate<-annotate_figure(p.arrange,
                top = text_grob("Nowcasting via boletins epidemiológicos do ministério da Saúde", color = "black", face = "bold", size = 14)
)
p.annotate
ggsave(p.annotate, filename = "./analise_UOL/plots/annotate_arrange_nowcasting_29_05.png", 
       dpi = 600, width = 9, height = 7)

# uol_final5<-uol_final4[,-c(2:5)]
# write.csv(uol_final4, file = "./analise_UOL/spreasheet_e_CSV/uol_final_nowcasting_29_05.csv", row.names = FALSE)


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
  