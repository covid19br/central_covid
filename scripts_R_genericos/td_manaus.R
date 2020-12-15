library(tidyverse)
library(ggpubr)
library(ISOweek)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
source("../nowcasting/fct/get.last.date.R")
source("../nowcasting/fct/read.sivep.R")

PRJROOT <- rprojroot::find_root(".here")

# Sources functions
devtools::load_all("../now_fcts/R/") ##loading de funções necessárias##

data.dir <- "../site/dados/municipios/SP/Sao_Paulo/tabelas_nowcasting_para_grafico/"
last.date <- get.last.date(data.dir)

# td_covid<-read_csv(paste0(data.dir, "tempo_duplicacao_covid_", last.date, ".csv"))
# td_srag<-read_csv(paste0(data.dir, "tempo_duplicacao_srag_", last.date, ".csv"))

nowcasted_covid<-read_csv(paste0(data.dir,"nowcasting_diario_covid_", last.date, ".csv"))
nowcasted_covid_est<-read_csv("../site/dados/estado/SP/tabelas_nowcasting_para_grafico/nowcasting_diario_covid_2020_10_13.csv")

r_efetivo_covid<-read_csv(paste0(data.dir, "r_efetivo_covid_", last.date,".csv"))
r_efetivo_covid_estado_sp<-read_csv("../site/dados/estado/SP/tabelas_nowcasting_para_grafico/r_efetivo_covid_2020_10_13.csv")

p.plot.casos.mun.sp.covid<-ggplot(data = r_efetivo_covid, aes(x=data, y=Mean.R, col = "Covid"))+
  geom_ribbon(aes(ymin = Quantile.0.025.R, ymax = Quantile.0.975.R), fill="lightgrey", col = "lightgrey") +
  geom_line(size = 1.25, color="darkblue") +
  scale_x_date( date_labels = "%d/%b", name="", date_breaks = "1 month") +
  geom_hline(yintercept=1, linetype="dashed", col="red") +          
  ylab("Número de reprodução") +
  theme_bw()+
  labs(title = "R efetivo - Município de São Paulo", 
       x = "Data",
       y = "Número de Reprodução da epidemia")+
  theme(legend.position = "none")
p.plot.casos.mun.sp.covid

ggsave(p.plot.casos.mun.sp.covid, filename = "../../../Imagens/r_efefivo_mun_sp.png", dpi = 600, width = 9, height = 7)

plot.nowcasted.covid.mun.sp <-
  ggplot(data=nowcasted_covid, aes(x=data, y=estimate.merged, ymin=lower.merged.pred, ymax=upper.merged.pred)) +
  geom_line(col = wong_black_light, linetype = 3, size = 0.25, alpha = 0.5)+
  # confianca
  geom_ribbon(fill=ibm_ultramarine_light, alpha = 0.25) +
  # previsao
  geom_line(aes(y=estimate.merged.smooth),
            col=wong_black_light) +
  geom_point(aes(y=n.casos),
             size=1,
             col=ibm_ultramarine_dark) +
  # estimados
  geom_point(aes(y=estimate),
             size=.75,
             col="red") +
  # escala
  scale_x_date(date_labels = "%d/%b", name="", date_breaks = "1 month") +
  theme_bw()+
  labs(title = "Nº Casos - Município de São Paulo", 
       x = "Data",
       y = "Número de Casos confirmados")+
  theme(legend.position = "none")
plot.nowcasted.covid.mun.sp

ggsave(plot.nowcasted.covid.mun.sp, filename = "../../../Imagens/nowcasted_mun_sp.png", dpi = 600, width = 9, height = 7)

plot.nowcasted.covid.est.sp<-ggplot(data=nowcasted_covid_est, aes(x=data, y=estimate.merged, ymin=lower.merged.pred, ymax=upper.merged.pred)) +
  geom_line(col = wong_black_light, linetype = 3, size = 0.25, alpha = 0.5)+
  # confianca
  geom_ribbon(fill=ibm_ultramarine_light, alpha = 0.25) +
  # previsao
  geom_line(aes(y=estimate.merged.smooth),
            col=wong_black_light) +
  geom_point(aes(y=n.casos),
             size=1,
             col=ibm_ultramarine_dark) +
  # estimados
  geom_point(aes(y=estimate),
             size=.75,
             col="red") +
  # escala
  scale_x_date(date_labels = "%d/%b", name="", date_breaks = "1 month") +
  theme_bw()+
  labs(title = "Nº Casos - Estado de São Paulo", 
       x = "Data",
       y = "Número de Casos confirmados")+
  theme(legend.position = "none")
plot.nowcasted.covid.est.sp

ggsave(plot.nowcasted.covid.est.sp, filename = "../../../Imagens/nowcasted_est_sp.png", dpi = 600, width = 9, height = 7)

p.plot.casos.est.sp.covid<-ggplot(data = r_efetivo_covid_estado_sp, aes(x=data, y=Mean.R, col = "Covid"))+
  geom_ribbon(aes(ymin = Quantile.0.025.R, ymax = Quantile.0.975.R), fill="lightgrey", col = "lightgrey") +
  geom_line(size = 1.25, color="darkblue") +
  scale_x_date( date_labels = "%d/%b", name="", date_breaks = "1 month") +
  geom_hline(yintercept=1, linetype="dashed", col="red") +          
  ylab("Número de reprodução") +
  theme_bw()+
  labs(title = "R efetivo - Estado de São Paulo", 
       x = "Data",
       y = "Número de Reprodução da epidemia")+
  theme(legend.position = "none")
p.plot.casos.est.sp.covid

ggsave(p.plot.casos.est.sp.covid, filename = "../../../Imagens/r_efefivo_est_sp.png", dpi = 600, width = 9, height = 7)
