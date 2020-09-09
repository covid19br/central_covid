##################################
# TESTES ESTADO DE SP
##################################
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(zoo)
library(aweek)
library(stringr)
library(RCurl)
library(car)
library(bbmle)
library(purrr)
library(devtools)
library(lubridate)
library(readr)
load_all("./now_fcts/")

dir<-"./dados/estado_SP/SRAG_hospitalizados/dados/SRAGH_2020_08_27.zip"

sivep_sp<-read.sivep.generica("./dados/estado_SP/SRAG_hospitalizados/dados/SRAGH_2020_08_27.zip")
sivep_br<-read.sivep.generica("./dados/SIVEP-Gripe/SRAGHospitalizado_2020_08_24.zip")

trash<-sivep_sp %>% 
  group_by(dt_coleta) %>% 
  summarize(N=n()) %>% 
  as.data.frame()

teste_positivo<-sivep_sp %>% 
  filter(pcr_sars2 == 1 & !is.na(dt_coleta) & dt_coleta <= "2020-08-27") %>% 
  group_by(dt_coleta) %>% 
  summarize(N=n()) %>% 
  as.data.frame()

teste_total<-sivep_sp %>% 
  filter(dt_coleta >= "2020-02-26" & !is.na(dt_coleta) & dt_coleta <= "2020-08-27") %>% 
  group_by(dt_coleta) %>% 
  summarize(N=n()) %>% 
  as.data.frame()

write.csv(teste_total, file = "./scripts_R_genericos/testes_totais_sp.csv", row.names = FALSE)
write.csv(teste_positivo, file = "./scripts_R_genericos/testes_positivos_sp.csv", row.names = FALSE)

total_zoo<-zoo(teste_total)
positivo_zoo<-zoo(teste_positivo)

teste_merged<-merge.zoo(window(total_zoo, start = "2020-02-26", end = "2020-08-27"), 
                        window(positivo_zoo, start = "2020-02-26", end = "2020-08-27"))

p.plot<-ggplot(data = teste_total, aes(x=dt_coleta, y=N))+
  geom_line(col = "red")+
  geom_line(data = teste_positivo, aes(x=dt_coleta, y=N), col = "blue")+
  labs(title = "Exames Realizados")+
  xlab("Data")+
  ylab("Nº total de exames")+
  theme_bw()
p.plot
ggarrange()