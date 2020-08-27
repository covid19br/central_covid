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

setwd("~/Área de Trabalho/central_covid/")

PRJROOT  = rprojroot::find_root(".here")

devtools::load_all("./now_fcts/R/") ##loading de funções necessárias##

source("./nowcasting/fct/median_delay.R")


## From Sivep directly ##
sivep_direct<-read_csv("./analise_UOL/dados/extract_dates_sivep_10_08.csv")
sivep_direct<-sivep_direct%>%
  mutate(dt_sin_pri = as.Date(dt_sin_pri, "%Y-%m-%d"),
         dt_encerra = as.Date(dt_encerra, "%Y-%m-%d"),
         dt_evoluca = as.Date(dt_evoluca, "%Y-%m-%d"))%>%
  as.data.frame()
sivep_series<-sivep_direct %>%
  group_by(dt_evoluca)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()
sivep_series_sin<-sivep_direct %>%
  group_by(dt_sin_pri)%>%
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()
trim<-0
### Data Evolução ###
nowcasting_direct<-NobBS.posterior2(data = sivep_direct,
                                    now = max(sivep_direct$dt_evoluca)-trim,
                                    onset_date = "dt_evoluca",
                                    report_date = "dt_encerra",
                                    units = "1 day",
                                    specs = list(nAdapt = 20000, nBurnin = 3000, nThin = 1, nSamp = 10000))
betas_direct<-beta.summary(nowcasting_direct) #### função em funcoes.R`
betas_cumsum_direct<-beta.cumsum(nowcasting_direct, samples = 5000)
nowcasting_cumsum_direct<-nowcasting.cumsum(nowcasting_direct, samples = 5000)

last_data<-max(sivep_direct$dt_evoluca)
ifelse(trim>0,
       trimmed<-paste0("_trimmed_",trim),trimmed<-NULL)

write.csv(nowcasting_direct$estimates, 
          file = paste0("./analise_UOL/output/nowcasting_estimates_direct_", last_data,trimmed,".csv"), row.names = FALSE)
write.csv(betas_direct, 
          file = paste0("./analise_UOL/output/betas_direct_", last_data,trimmed,".csv"), row.names = FALSE)
write.csv(betas_cumsum_direct, 
          file = paste0("./analise_UOL/output/betas_cumsum_direct_",last_data,trimmed,".csv"), row.names = FALSE)
write.csv(nowcasting_cumsum_direct, 
          file = paste0("./analise_UOL/output/nowcasting_cumsum_direct_", last_data,trimmed,".csv"), row.names = FALSE)
