##################################
########Análise UOL###############
##################################
######TESTE DE SENSIBILIDADE######
##################################
##################################
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

PRJROOT  = rprojroot::find_root(".here")

source("./funcoes.R")
source("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/_src/funcoes.R")

data_ultimo_boletim<-as.Date("2020-05-08")

uol<-read.csv2("./analise_UOL/SRAGs-tabela-last-updated_revised2.csv", as.is = TRUE, sep = ",")

lista_boletins<-list()
for (i in 2:25) {
  lista_boletins[[i]]<-uol[1:length(uol[,i]), c(1,i:ncol(uol))]
}
names(lista_boletins)[2:25]<-colnames(uol[,-1])[1:24]
lista_boletins<-lista_boletins[[2:25]]

uol2<-uol[1:31, c(1,15:26)]
uol2[is.na(uol2)] <- 0

uol2.1<-uol2

for(i in 3:length(uol2)){
  for(k in 1:dim(uol2)[1]){
    if(uol2.1[k,i]>uol2.1[k,(i-1)] & uol2.1[k,i]>0){
      uol2.1[k,i]=uol2.1[k,(i-1)]
    }
  }
}
uol2<-uol2.1
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
now_csv<-write.csv(nowcasting6$estimates, 
                   file = paste("analise_UOL/spreasheet_e_CSV/nowcasting_", data_usada, ".csv",sep = ""), 
                   row.names = FALSE)

uol_last<-uol[1:31,1:2]
names(uol_last)<-c("Data", "Ultimo")
uol_last$Data<-as.Date(uol_last$Data, "%d/%m/%Y")

p.prev.ic <- ggplot(nowcasting6$estimates, aes(x = onset_date, y = estimate)) +
  geom_line(data = uol_df2, aes(x = Death_date, y = N,
            color = "Boletim dia 14 de abril"), lwd = 1) +
  geom_line(data = uol_last, aes(x = Data, y = Ultimo, color = "Boletim dia 8 de maio"), lwd = 1.5)+
  geom_line(aes(x = onset_date, y = n.reported), col = "blue", lwd = 1.2)+
  geom_line(aes(col = "Nowcasting com 14 de abril")) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha =0.15, fill = "indianred3") +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos") +
  theme_bw() +
  # theme(legend.position = "none") +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("blue", "orange", "indianred3"), aesthetics = c("colour", "fill"))
  # ggtitle("Nowcasting de óbitos de COVID-19")
p.prev.ic

now_df<-nowcasting6$estimates

now_cumsum<-apply(t(now_df[,-4]), 1, cumsum)
now_cumsum<-as.data.frame(now_cumsum)
names(now_cumsum)<-c("estimate cumsum", "lower cumsum", "upper cumsum", "n.reported cumsum")
now_cumsum<-as.data.frame(cbind(now_cumsum, now_df))

uol_last_cumsum<-apply(t(uol_last$Ultimo), 1, cumsum)
uol_last_cumsum<-as.data.frame(uol_last_cumsum)
uol_last_cumsum<-as.data.frame(cbind(uol_last_cumsum, uol_last))
names(uol_last_cumsum)<-c("ultimo cumsum", "Data", "ultimo")

uol_df2_cumsum<-apply(t(uol_df2$N), 1, cumsum)
uol_df2_cumsum<-as.data.frame(uol_df2_cumsum)
uol_df2_cumsum<-as.data.frame(cbind(uol_df2_cumsum, uol_df2))
names(uol_df2_cumsum)<-c("cumsum", "Death_date", "N")

now_cumsum<-cbind(now_cumsum, ultimo = uol_last_cumsum$`ultimo cumsum`)
write.csv(now_cumsum, file = "./analise_UOL/spreasheet_e_CSV/nowcumsum_sensibilidade", row.names = FALSE)

p.prev.ic.cumsum <- ggplot(now_cumsum, aes(x = onset_date, y = `estimate cumsum`)) +
  geom_line(data = uol_df2_cumsum, aes(x = Death_date, y = `cumsum`,
                                color = "Boletim dia 14 de abril"), lwd = 1) +
  geom_line(data = uol_last_cumsum, aes(x = Data, y = `ultimo cumsum`, color = "Boletim dia 8 de maio"), lwd = 1.5)+
  geom_line(aes(x = onset_date, y = `n.reported cumsum`), col = "blue", lwd = 1.2)+
  geom_line(aes(col = "Nowcasting com 14 de abril")) +
  geom_ribbon(aes(ymin = `lower cumsum`, ymax = `upper cumsum`), alpha =0.15, fill = "indianred3") +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos") +
  theme_bw() +
  # theme(legend.position = "none") +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("blue", "orange", "indianred3"), aesthetics = c("colour", "fill")) 
  # ggtitle("Nowcasting de óbitos de COVID-19")
p.prev.ic.cumsum
ggsave(plot = p.prev.ic.cumsum, filename = "./analise_UOL/plots/sensibilidade_nowcasting_BE_MS_08_05_cumsum.png", dpi = 600, width = 9, height = 7)

###########################
######SALVANDO EM SVG######
###########################
# plots.para.atualizar<-p.prev.ic
# filepath<-"./analise UOL/plots/plots SVG/sensibilidade_nowcasting_BE_MS_08_maio"
plots.para.atualizar<-p.prev.ic.cumsum
filepath<-"./analise_UOL/plots/plots_SVG/sensibilidade_nowcasting_BE_MS_08_maio_cumsum"

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

