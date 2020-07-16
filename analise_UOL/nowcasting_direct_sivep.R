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
## From Sivep directly ##
sivep_direct<-read_csv("./analise_UOL/dados/extract_dates_sivep_14_julho.csv")
sivep_direct<-sivep_direct%>%
  mutate(dt_encerra = as.Date(dt_encerra, "%Y-%m-%d"),
         dt_evoluca = as.Date(dt_evoluca, "%Y-%m-%d"))%>%
  as.data.frame()
sivep_series<-sivep_direct %>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()
nowcasting_direct<-NobBS.posterior2(data = sivep_direct,
                                    now = max(sivep_direct$dt_evoluca)-trim,
                                    onset_date = "dt_evoluca",
                                    report_date = "dt_encerra",
                                    units = "1 day",
                                    specs = list(nAdapt = 15000, nBurnin = 3000, nThin = 1, nSamp = 10000))
betas_direct<-beta.summary(nowcasting_direct) #### função em funcoes.R`
betas_cumsum_direct<-beta.cumsum(nowcasting_direct, samples = 5000)
nowcasting_cumsum_direct<-nowcasting.cumsum(nowcasting_direct, samples = 5000)

#############################
######### Gráficos ##########
#############################

################################################################################
## Plots: objetos ggplot2
################################################################################
p.betas.direct<-
  ggplot(betas_direct, aes(atraso, mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  xlab("Dias após dia do óbito") +
  ylab("Probabilidade de notificação") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Atraso de notificação de óbitos COVID por SRAG")
p.betas.direct

p.betas_cumsum_direct <- p.betas.direct %+% betas_cumsum_direct +
  ylab("Probabilidade Acumulada\n de notificação")
p.betas_cumsum_direct

p.prev.ic_direct<-nowcasting_direct$estimates %>% 
  ggplot(aes(x = onset_date, y = estimate)) +
  geom_line(data = sivep_series, aes(x = dt_evoluca, y = N, color="Notificados"), lwd = 1.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos por dia") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill"))+
  ggtitle("Diários")
p.prev.ic_direct

p.prev.ic.cumsum_direct<-
  ggplot(nowcasting_cumsum_direct, aes(x= Dates, y = mean))+
  geom_line(data = sivep_series, aes(x = dt_evoluca, y = Cum, color="Notificados"), lwd = 1.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos Acumulados") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "blue"), aesthetics = c("colour", "fill"))+
  ggtitle("Acumulados Diários")
p.prev.ic.cumsum_direct

p.arrange<-ggpubr::ggarrange(p.betas.direct, p.betas_cumsum_direct, p.prev.ic_direct, p.prev.ic.cumsum_direct)
p.arrange
ggsave(p.arrange, filename = "./analise_UOL/plots/arrange_nowcasting_07_14.png", 
       dpi = 600, width = 9, height = 7)