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
uol_melted<-reshape::melt(uol, id.vars = "Data")
diff_BE2<-c()
uol_diff<-uol[,-1]
for (i in ncol(uol_diff):2) {
  diff_BE<-c()
  diff_BE<-as.numeric(uol_diff[,i-1]-uol_diff[,i])
  diff_BE<-data.frame(N = diff_BE, Data = uol$Data)
  # diff_BE<-diff_BE%>%
  #   filter(!is.na(N))%>% ## Porque se não ele não faz o cumsum ##
  #   mutate(cumsum = cumsum(N))%>%
  #   as.data.frame()   
  diff_BE$id<-colnames(uol_diff)[i]
  diff_BE2<-rbind(diff_BE2,diff_BE)
}

diff_BE2<-ldply(diff_BE2$N,.id = !is.na(diff_BE2$N),cumsum)

p.uol <-
  ggplot(diff_BE2, aes(x = Data, y = N, col = id)) +
  # geom_point(shape = 1)+
  # geom_line()+
  geom_line(aes(x=Data, y=cumsum, col=id))+
  geom_vline(xintercept = as.Date("2020-03-17", format = "%Y-%m-%d"), colour = "indianred3", size = 0.45, linetype = "dashed")+
  scale_color_viridis(name = "Nº Boletim", 
                        option = "viridis", 
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
