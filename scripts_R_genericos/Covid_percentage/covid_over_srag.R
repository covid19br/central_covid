library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(wesanderson)
library("cowplot")
library("plotly")
library("gganimate")
library("RColorBrewer")
library("transformr")
library("readr")
library(geofacet)

setwd("~/Área de Trabalho/central_covid/")

source("scripts_R_genericos/comparacao_bases/format_all_nowcasting.R")
source("nowcasting/fct/00_load_libraries.R")

pal_wes<-wes_palette("Zissou1", 10, "continuous")

srag_est<-read_csv("dados_processados/integridade_SIVEP/dados_srag_est.csv")
covid_est<-read_csv("dados_processados/integridade_SIVEP/dados_covid_est.csv")

srag_filtered<-srag_est %>% 
  filter(dt_sin_pri >= "2020-01-01" & !is.na(sg_uf)) %>%
  select(dt_sin_pri, sg_uf, n, data) %>% 
  as.data.frame()
covid_filtered<-covid_est %>% 
  filter(dt_sin_pri >= "2020-01-01" & !is.na(sg_uf)) %>% 
  select(dt_sin_pri, sg_uf, n, data) %>% 
  as.data.frame()

joint_filtered<-left_join(srag_filtered, covid_filtered, by = c("dt_sin_pri", "sg_uf", "data"))
joint_filtered<-joint_filtered %>% 
  mutate(covid_over_srag = n.y/n.x) %>% 
  as.data.frame()

joint_filtered<-joint_filtered %>%
  filter(data == max(data)) %>%
  as.data.frame()

p.plot<-ggplot(data = joint_filtered)+
  geom_col(aes(x=dt_sin_pri, y=n.x, fill = covid_over_srag), 
           width = 1, na.rm = TRUE, position = "stack")+
  theme_minimal()+
  labs(x = "Data de 1º Sintomas", 
       y = "Nº SRAG Cases")+
  scale_fill_gradientn(name = "SRAG by Covid/SRAG",
                       colors = pal_wes)+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_geo(~sg_uf, grid = "br_states_grid1", scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b") 
  # transition_states(factor(data))
p.plot

animate(p.plot,
        duration = 20,
        end_pause = 5,
        width = 800,
        height = 600)

########################################################
####  Fazendo com os nowcastings #######################
########################################################
# Libraries
library(widgetframe)
library(tidyverse)
library(lubridate)
library(optparse)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(withr)

estados <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG",
             "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR",
             "RS", "SC", "SE", "SP", "TO")


df.estados <- data.frame(estado=estados, data.dir=NA, data=NA, existe.covid=NA,
                         existe.srag=NA, existe.ob.covid=NA, existe.ob.srag=NA,
                         row.names=estados)
source("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/_src/fct/funcoes.R")
source("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/_src/fct/plot.nowcast.diario.brasil.R")
source("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/_src/fct/existe.nowcasting2.R")
source("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/_src/fct/makeNamedList.R")
source("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/_src/fct/plot.formatos.R")

for (estado in estados) {
  # dir para os ler os dados
  data.dir <- paste0("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/dados/estado/", 
                     estado,
                     "/tabelas_nowcasting_para_grafico/")
  
  # pegando a data mais recente
  data <- "2021_02_08"
  
  # testando se existe nowcasting
  existe.covid <- existe.nowcasting2(tipo = "covid",
                                     output.dir = data.dir,
                                     data = data)
  existe.srag <- existe.nowcasting2(tipo = "srag",
                                    output.dir = data.dir,
                                    data = data)
  existe.ob.covid <- existe.nowcasting2(tipo = "obitos_covid",
                                        output.dir = data.dir,
                                        data = data)
  existe.ob.srag <- existe.nowcasting2(tipo = "obitos_srag",
                                       output.dir = data.dir,
                                       data = data)
  df.estados[estado,] <- c(estado, data.dir, data, existe.covid, existe.srag,
                           existe.ob.covid, existe.ob.srag)
}

# Covid
df.covid.diario <- list()
for (estado in estados) {
    data.dir <- df.estados[estado, "data.dir"]
    df.covid.diario[[estado]] <- read_csv(paste0(data.dir, "nowcasting_diario_covid_", data, ".csv"))
}
df.covid.diario <- plyr::ldply(df.covid.diario, .id="UF")
plot.nowcast.covid <- plot.nowcast.diario.brasil(df.covid.diario)

# SRAG
df.srag.diario <- list()
for (estado in estados) {
    data.dir <- df.estados[estado, "data.dir"]
    df.srag.diario[[estado]] <- read_csv(paste0(data.dir, "nowcasting_diario_srag_", data, ".csv"))
}
df.srag.diario <- plyr::ldply(df.srag.diario, .id="UF")
plot.nowcast.srag <- plot.nowcast.diario.brasil(df.srag.diario)

# óbitos Covid
df.ob.covid.diario <- list()
for (estado in estados) {
    data.dir <- df.estados[estado, "data.dir"]
    df.ob.covid.diario[[estado]] <- read_csv(paste0(data.dir, "nowcasting_diario_obitos_covid_", data, ".csv"))
}
df.ob.covid.diario <- plyr::ldply(df.ob.covid.diario, .id="UF")
plot.nowcast.ob.covid <- plot.nowcast.diario.brasil(df.ob.covid.diario)

# óbitos SRAG
df.ob.srag.diario <- list()
for (estado in estados) {
    data.dir <- df.estados[estado, "data.dir"]
    df.ob.srag.diario[[estado]] <- read_csv(paste0(data.dir, "nowcasting_diario_obitos_srag_", data, ".csv"))
}
df.ob.srag.diario <- plyr::ldply(df.ob.srag.diario, .id="UF")
plot.nowcast.ob.srag <- plot.nowcast.diario.brasil(df.ob.srag.diario)

casos_joint<-left_join(df.srag.diario[,c(1,2,5)], df.covid.diario[,c(1,2,5)], by = c("data", "UF"))
casos_joint<- casos_joint %>% 
  mutate(covid_over_srag = estimate.merged.y/estimate.merged.x) %>% 
  as.data.frame()
obitos_joint<-left_join(df.ob.srag.diario[,c(1,2,5)], df.ob.covid.diario[,c(1,2,5)], by = c("data", "UF"))
obitos_joint <- obitos_joint %>% 
  mutate(covid_over_srag = estimate.merged.y/estimate.merged.x) %>% 
  as.data.frame()


p.plot_casos_nowcasted<-ggplot(data = casos_joint)+
  geom_col( aes(x=data, y= estimate.merged.x, fill = covid_over_srag), 
            width = NULL, na.rm = TRUE, position = "stack")+
  theme_minimal()+
  labs(x = "Data de 1º Sintomas", 
       y = "Nº SRAG Cases")+
  scale_fill_gradientn(name = "SRAG by Covid/SRAG",
                       colors = pal_wes)+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_geo(~UF, grid = "br_states_grid1", scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b")
p.plot_casos_nowcasted
