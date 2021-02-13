library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(wesanderson)

source("scripts_R_genericos/comparacao_bases/format_all_nowcasting.R")

setwd("~/Área de Trabalho/central_covid/")

pal_wes<-wes_palette("Zissou1", 10, "continuous")

srag_est<-read_csv("dados_processados/integridade_SIVEP/dados_srag_est.csv")
covid_est<-read_csv("dados_processados/integridade_SIVEP/dados_covid_est.csv")

nowcasted<-format_nowcasting_output(dir = "dados_processados/nowcasting/",
                                         escala = "estado", geocode = 35,
                                         analise = c("nowcasting_díario"))

srag_filtered<-srag_est %>% 
  filter(dt_sin_pri >= "2020-01-01" & !is.na(sg_uf) & data == max(data)) %>%
  select(dt_sin_pri, sg_uf, n) %>% 
  as.data.frame()
covid_filtered<-covid_est %>% 
  filter(dt_sin_pri >= "2020-01-01" & !is.na(sg_uf) & data == max(data)) %>% 
  select(dt_sin_pri, sg_uf, n) %>% 
  as.data.frame()

joint_filtered<-left_join(srag_filtered, covid_filtered, by = c("dt_sin_pri", "sg_uf"))
joint_filtered<-joint_filtered %>% 
  mutate(covid_over_srag = n.y/n.x) %>% 
  as.data.frame()

joint_filtered2<-joint_filtered %>% 
  mutate(week_sin_pri = epiweek(dt_sin_pri)) %>% 
  select(week_sin_pri, sg_uf, n.x, covid_over_srag) %>% 
  as.data.frame()

p.plot<-ggplot(data = joint_filtered, aes(x=dt_sin_pri, y=n.x, fill = covid_over_srag))+
  geom_col(width = 1, na.rm = TRUE, position = position_stack(reverse = FALSE))+
  theme_bw()+
  labs(x = "Data de 1º Sintomas", 
       y = "SRAG Cases")+
  scale_fill_gradientn(name = "SRAG by Covid/SRAG",
                       colors = pal_wes)+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_wrap(sg_uf~., scales = "free_y", ncol = 3)
p.plot  

p.plot2<-ggplot(data = joint_filtered2, aes(x=week_sin_pri, y=n.x, fill = covid_over_srag))+
  geom_col(width = 1, na.rm = TRUE, position = position_stack(reverse = FALSE))+
  theme_bw()+
  labs(x = "Semana de 1º Sintomas", 
       y = "SRAG Cases")+
  scale_fill_gradientn(name = "SRAG by Covid/SRAG",
                       colors = pal_wes)+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_wrap(sg_uf~., scales = "free_y", ncol = 3)
p.plot2

