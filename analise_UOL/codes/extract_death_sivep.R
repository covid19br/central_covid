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

sivep_03_08<-read.sivep.generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_08_03.zip')
sivep_29<-read.sivep.generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_07_29.zip')
sivep_21<-read.sivep.generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_07_21.zip')
sivep_14<-read.sivep.generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_07_14.zip')
sivep_07<-read.sivep.generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_07_07.zip')
sivep_30<-read.sivep.generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_06_30.zip')
sivep_23<-read.sivep.generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_06_23.zip')
sivep_17<-read.sivep.generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_06_17.zip')
sivep_09<-read.sivep.generica(file.name = './dados/SIVEP-Gripe/SRAGHospitalizado_2020_06_09.zip')

## compilando e salvando em csv ##
sivep_03_08_obitos<-sivep_03_08 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
  filter(evolucao == 2) %>%
  filter(!is.na(dt_evoluca)) %>%
  mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                           na.rm = TRUE)) %>%
  select(dt_sin_pri, dt_evoluca, dt_notific, dt_encerra)%>%
  as.data.frame()
write.csv(sivep_03_08_obitos, "~/Área de Trabalho/central_covid/analise_UOL/dados/extract_dates_sivep_03_08.csv", row.names = FALSE)
sivep_03_08_obitos_sum<-sivep_03_08_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()
sivep_03_08_sin_sum<-sivep_03_08_obitos%>%
  group_by(dt_sin_pri)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_03_08_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_03_08.csv")

sivep_29_obitos<-sivep_29 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
  filter(evolucao == 2) %>%
  filter(!is.na(dt_evoluca)) %>%
  mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                           na.rm = TRUE)) %>%
  select(dt_sin_pri, dt_evoluca, dt_notific, dt_encerra)%>%
  as.data.frame()
write.csv(sivep_29_obitos, "~/Área de Trabalho/central_covid/analise_UOL/dados/extract_dates_sivep_29_julho.csv", row.names = FALSE)
sivep_29_obitos_sum<-sivep_29_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()
sivep_29_sin_sum<-sivep_29_obitos%>%
  group_by(dt_sin_pri)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_29_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_29_julho.csv")

sivep_21_obitos<-sivep_21 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
  filter(evolucao == 2) %>%
  filter(!is.na(dt_evoluca)) %>%
  mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                           na.rm = TRUE)) %>%
  select(dt_sin_pri, dt_evoluca, dt_notific, dt_encerra)%>%
  as.data.frame()
write.csv(sivep_21_obitos, "~/Área de Trabalho/central_covid/analise_UOL/dados/extract_dates_sivep_21_julho.csv", row.names = FALSE)
sivep_21_obitos_sum<-sivep_21_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()
sivep_21_sin_sum<-sivep_21_obitos%>%
  group_by(dt_sin_pri)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_21_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_21_julho.csv")

sivep_14_obitos<-sivep_14 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
  filter(evolucao == 2) %>%
  filter(!is.na(dt_evoluca)) %>%
  mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                           na.rm = TRUE)) %>%
  select(dt_sin_pri, dt_evoluca, dt_notific, dt_encerra)%>%
  as.data.frame()
write.csv(sivep_14_obitos, "~/Área de Trabalho/central_covid/analise_UOL/dados/extract_dates_sivep_14_julho.csv", row.names = FALSE)
sivep_14_obitos_sum<-sivep_14_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()
sivep_14_sin_sum<-sivep_14_obitos%>%
  group_by(dt_sin_pri)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_14_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_14_julho.csv")

sivep_07_obitos<-sivep_07 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
  filter(evolucao == 2) %>%
  filter(!is.na(dt_evoluca)) %>%
  mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                           na.rm = TRUE)) %>%
  select(dt_evoluca, dt_notific, dt_encerra)%>%
  as.data.frame()
write.csv(sivep_07_obitos, "~/Área de Trabalho/central_covid/analise_UOL/dados/extract_dates_sivep_07_julho.csv", row.names = FALSE)
sivep_07_obitos_sum<-sivep_07_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_07_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_07_julho.csv")


sivep_30_obitos<-sivep_30 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
  filter(evolucao == 2) %>%
  filter(!is.na(dt_evoluca)) %>%
  mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                           na.rm = TRUE)) %>%
  select(dt_evoluca, dt_notific, dt_encerra)%>%
  as.data.frame()
write.csv(sivep_30_obitos, "~/Área de Trabalho/central_covid/analise_UOL/dados/extract_dates_sivep_30_junho.csv", row.names = FALSE)
sivep_30_obitos_sum<-sivep_30_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_30_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_30_junho.csv")

sivep_23_obitos<-sivep_23 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
  filter(evolucao == 2) %>%
  filter(!is.na(dt_evoluca)) %>%
  mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                           na.rm = TRUE)) %>%
  select(dt_evoluca, dt_notific, dt_encerra)%>%
  as.data.frame()
sivep_23_obitos_sum<-sivep_23_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_23_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_23_junho.csv")

sivep_17_obitos<-sivep_17 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
  filter(evolucao == 2) %>%
  filter(!is.na(dt_evoluca)) %>%
  mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                           na.rm = TRUE)) %>%
  select(dt_evoluca, dt_notific, dt_encerra)%>%
  as.data.frame()
sivep_17_obitos_sum<-sivep_17_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_17_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_17_junho.csv")

sivep_09_obitos<-sivep_09 %>%
  filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
  filter(evolucao == 2) %>%
  filter(!is.na(dt_evoluca)) %>%
  mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                           na.rm = TRUE)) %>%
  select(dt_evoluca, dt_notific, dt_encerra)%>%
  as.data.frame()
sivep_09_obitos_sum<-sivep_09_obitos%>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

write.csv(sivep_09_obitos_sum,"~/Área de Trabalho/central_covid/analise_UOL/dados/spreasheet_e_CSV/extract_09_junho.csv")
