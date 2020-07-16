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
diff_se_maior <- function(x, y) {
  z <- if_else(y >= x,  y - x, 0)
  return(z)
}

data_wide<-uol[,-1]
for (i in (ncol(data_wide)):2) {
  data_diff[,i] <- diff_se_maior(data_wide[,i-1], data_wide[,i])
}

dados_mun <- data_diff %>%
  rownames_to_column(var = "data_inicio") %>%
  pivot_longer(cols = -1, names_to = "data_base") %>%
  filter(value != 0) %>%
  mutate(data_inicio = lubridate::as_date(data_inicio),
         data_base = lubridate::as_date(data_base)) %>%
  data.frame()
