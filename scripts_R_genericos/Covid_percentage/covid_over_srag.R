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

source("scripts_R_genericos/comparacao_bases/format_all_nowcasting.R")
source("nowcasting/fct/00_load_libraries.R")

pal_wes<-wes_palette("Zissou1", 10, "continuous")

## Carregando CSV sem nowcasting
### Casos
srag_est<-read_csv("dados_processados/integridade_SIVEP/dados_srag_est.csv")
covid_est<-read_csv("dados_processados/integridade_SIVEP/dados_covid_est.csv")
### Obitos
srag_obs_est<-read_csv("dados_processados/integridade_SIVEP/dados_obsrag_est.csv")
covid_obs_est<-read_csv("dados_processados/integridade_SIVEP/dados_obcovid_est.csv")

### Casos
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

### Obitos
srag_obs_filtered<-srag_obs_est %>% 
  filter(dt_evoluca >= "2020-01-01" & !is.na(sg_uf)) %>%
  select(dt_evoluca, sg_uf, n, data) %>% 
  as.data.frame()
covid_obs_filtered<-covid_obs_est %>% 
  filter(dt_evoluca >= "2020-01-01" & !is.na(sg_uf)) %>% 
  select(dt_evoluca, sg_uf, n, data) %>% 
  as.data.frame()

joint_obs_filtered<-left_join(srag_obs_filtered, covid_obs_filtered, by = c("dt_evoluca", "sg_uf", "data"))
joint_obs_filtered<-joint_obs_filtered %>% 
  mutate(covid_over_srag = n.y/n.x) %>% 
  as.data.frame()

### Casos
#### Pixel Plot
p.casos.raw<-
  joint_filtered %>% 
  filter(data == max(data)) %>%
  ggplot(aes(x=dt_sin_pri, y=n.x, fill = covid_over_srag))+
  geom_col(width = 1, na.rm = TRUE, position = position_dodge())+
  theme_minimal()+
  labs(x = "1º symptoms Date", 
       y = "Nº SRAG Cases",
       title = "Cases Rawdata")+
  scale_fill_gradientn(name = "SRAG by Covid/SRAG",
                       colors = pal_wes)+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_geo(~sg_uf, grid = "br_states_grid1", scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b")
p.casos.raw

ggsave("scripts_R_genericos/Covid_percentage/img/cases_rawdata.png",
       plot = p.casos.raw,
       dpi = 300, 
       height = 9,
       width = 9)


### Obitos
#### Pixel Plot
p.obitos.raw<-
  joint_obs_filtered %>% 
  filter(data == max(data)) %>%
  ggplot(aes(x=dt_evoluca, y=n.x, fill = covid_over_srag))+
  geom_col(width = 1, na.rm = TRUE, position = "stack")+
  theme_minimal()+
  labs(x = "Death Date", 
       y = "Nº SRAG Deaths",
       title = "Deaths Rawdata")+
  scale_fill_gradientn(name = "SRAG by Covid/SRAG",
                       colors = pal_wes)+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_geo(~sg_uf, grid = "br_states_grid1", scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b")
p.obitos.raw

ggsave("scripts_R_genericos/Covid_percentage/img/deaths_rawdata.png",
       plot = p.obitos.raw,
       dpi = 300, 
       height = 9,
       width = 9)


## Gifs##

### Cases
g.casos<- joint_filtered %>% 
  ggplot(aes(x=dt_sin_pri, y=n.x, fill = covid_over_srag))+
  geom_col(width = 1, na.rm = TRUE, position = "stack")+
  theme_minimal()+
  labs(x = "1º symptoms Date", 
       y = "Nº SRAG Cases",
       title = "Cases")+
  scale_fill_gradientn(name = "SRAG by Covid/SRAG",
                       colors = pal_wes)+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_geo(~sg_uf, grid = "br_states_grid1", scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b")+
  transition_states(factor(data))
g.casos

animate(g.casos,
        duration = 20,
        end_pause = 5,
        width = 800,
        height = 600)

### Obitos
g.obitos<- joint_obs_filtered %>% 
  ggplot(aes(x=dt_evoluca, y=n.x, fill = covid_over_srag))+
  geom_col(width = 1, na.rm = TRUE, position = "stack")+
  theme_minimal()+
  labs(x = "Death Date", 
       y = "Nº SRAG Deaths",
       title = "Deaths")+
  scale_fill_gradientn(name = "SRAG by Covid/SRAG",
                       colors = pal_wes)+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_geo(~sg_uf, grid = "br_states_grid1", scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b")+
  transition_states(factor(data))
g.obitos

animate(g.obitos,
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
library(scales)

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

casos_joint<-merge(df.srag.diario, df.covid.diario, 
                   by = c("data", "UF"), 
                   suffixes = c(".srag", ".covid"))
casos_joint<- casos_joint %>% 
  mutate(covid_over_srag = estimate.merged.covid/estimate.merged.srag,
         upper_covid_over_srag = upper.merged.pred.covid/upper.merged.pred.srag, ## PRECISA VERIFICAR ESSA CONTA!!##
         lower_covid_over_srag = lower.merged.pred.covid/lower.merged.pred.srag) %>% ## PRECISA VERIFICAR ESSA CONTA!!##
  as.data.frame()

obitos_joint<-merge(df.ob.srag.diario, df.ob.covid.diario, 
                    by = c("data", "UF"),
                    suffixes = c(".srag", ".covid"))
obitos_joint <- obitos_joint %>% 
  mutate(covid_over_srag = (estimate.merged.covid/estimate.merged.srag),
         upper_covid_over_srag = upper.merged.pred.covid/upper.merged.pred.srag, ## PRECISA VERIFICAR ESSA CONTA!!##
         lower_covid_over_srag = lower.merged.pred.covid/lower.merged.pred.srag) %>% ## PRECISA VERIFICAR ESSA CONTA!!##
  as.data.frame()

p.casos.nowcasted<-casos_joint %>% 
  filter(UF != "AC") %>%
  ggplot(aes(x = data, 
             fill = covid_over_srag, 
             y = estimate.merged.srag))+
  geom_col(width = 7.0, position = position_dodge())+
  scale_fill_gradientn(name = "SRAG by Covid/SRAG",
                       colors = pal_wes)+
  theme_minimal()+
  labs(x = "First symptoms Date", 
       y = "Nº SRAG Cases",
       title = "Cases Nowcasted")+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_geo(~UF, grid = "br_states_grid1", scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b")
p.casos.nowcasted

## Tudo
ggsave("scripts_R_genericos/Covid_percentage/img/casos_nowcasted.png",
       plot = p.casos.nowcasted,
       dpi = 300, 
       height = 9,
       width = 9)
## Sem Acre
ggsave("scripts_R_genericos/Covid_percentage/img/casos_nowcasted_no_acre.png",
       plot = p.casos.nowcasted,
       dpi = 300, 
       height = 9,
       width = 9)

p.obitos.nowcasted<-obitos_joint %>% 
  ggplot(aes(x = data,
             fill = covid_over_srag,
             y = estimate.merged.srag))+
  geom_col(width = 7.0, position = position_dodge())+
  theme_minimal()+
  labs(x = "First symptoms Date", 
       y = "Nº SRAG Deaths",
       title = "Deaths Nowcasted")+
  scale_fill_gradientn(name = "SRAG by Covid/SRAG",
                       colors = pal_wes)+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_geo(~UF, grid = "br_states_grid1", scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b")
p.obitos.nowcasted

ggsave("scripts_R_genericos/Covid_percentage/img/deaths_nowcasted.png",
       plot = p.obitos.nowcasted,
       dpi = 300, 
       height = 9,
       width = 9)

p.casos.traj.nowcasted<-
  casos_joint %>% 
  filter(UF != "AC") %>%
  ggplot(aes(x = estimate.merged.srag, y = estimate.merged.covid))+
  geom_path(aes(colour = covid_over_srag))+
  theme_minimal()+
  scale_colour_gradientn(name = "SRAG by Covid/SRAG",
                        colors = pal_wes)+
  labs(x = "SRAG Cases",
       y = "SRAG by Covid Cases",
       title = "Trajectories Cases")+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_geo(~UF, grid = "br_states_grid1", scales = "free")
p.casos.traj.nowcasted

## Tudo
ggsave("scripts_R_genericos/Covid_percentage/img/trajectories_cases_nowcasted.png",
       plot = p.casos.traj.nowcasted,
       dpi = 300, 
       height = 9,
       width = 9)
## Sem Acre
ggsave("scripts_R_genericos/Covid_percentage/img/trajectories_cases_nowcasted_no_acre.png",
       plot = p.casos.traj.nowcasted,
       dpi = 300, 
       height = 9,
       width = 9)

p.obitos.traj.nowcasted<-
  obitos_joint %>% 
  ggplot(aes(x = estimate.merged.srag, y = estimate.merged.covid))+
  geom_path(aes(colour = covid_over_srag))+
  theme_minimal()+
  scale_colour_gradientn(name = "SRAG by Covid/SRAG",
                         colors = pal_wes)+
  labs(x = "SRAG Deaths",
       y = "SRAG by Covid Deaths",
       title = "Trajectories Deaths")+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_geo(~UF, grid = "br_states_grid1", scales = "free")
p.obitos.traj.nowcasted

ggsave("scripts_R_genericos/Covid_percentage/img/trajectories_deaths_nowcasted.png",
       plot = p.obitos.traj.nowcasted,
       dpi = 300, 
       height = 9,
       width = 9)


# DAQUI PRA BAIXO TENTATIVA DE SOMBRA DE CASOS COM GEOM_COL E 
# LINHAS PARA COVID OVER SRAG, O FAC, FATOR DE RESCALA DO EIXO SECUNDÁRIO
# ESTÁ ERRADO, FICA COM O MÁXIMO DE 200 PARA TODOS OS ESTADOS MESMO SENDO 
# CALCULADO INDEPENDEMENTE

p.plots.list<-list()
p.casos.merged<-list()
p.obitos.merged<-list()

for (i in unique(casos_joint$UF)) {
  p.casos.merged[[i]]<-casos_joint %>% 
    filter(UF == i) %>% 
    mutate(fac_srag = ceiling(max(estimate.merged.srag)/100)*100,
           bar_srag = estimate.merged.srag/fac_srag,
           bar_covid = estimate.merged.covid/fac_srag,
           ) %>%
    as.data.frame()
  # p.obitos.merged[[i]]<-obitos_joint %>%
  #   filter(UF == i) %>%
  #   mutate(fac_obs_srag = ceiling(max(estimate.merged.srag)/100)*100,
  #          bar_obs_srag = estimate.merged.srag/fac_obs_srag,
  #          bar_obs_covid = estimate.merged.covid/fac_obs_srag) %>%
  #   as.data.frame()
  fac_lab<-max(p.casos.merged[[i]]$fac_srag) ## É UM ABSURDO TER Q PASSAR UM NÚMERO ASSIM
  p.plots.list[[i]]<- 
    ggplot(data = p.casos.merged[[i]], aes(x = data))+
    geom_line(data = p.casos.merged[[i]],
              aes(y = covid_over_srag,
                  col = "SRAG by Covid/SRAG Cases"))+
    # geom_line(data = p.obitos.merged[[i]], 
    #           aes(y = covid_over_srag, 
    #               col = "SRAG by Covid/SRAG Deaths"))+
    geom_col(data = p.casos.merged[[i]],
             aes(y = bar_srag, fill = "SRAG Cases"), 
             width = 7,
             position = position_dodge(), 
             alpha = 0.5)+
    geom_col(data = p.casos.merged[[i]],
             aes(y = bar_covid, fill = "SRAG by Covid Cases"), 
             width = 7,
             position = position_dodge(), 
             alpha = 0.5)+
    labs(title = i,
         x = "First symptoms Date",
         y = "Number of Cases")+
    theme(legend.position = "bottom",
          plot.title = element_text(size = 14))+
    theme_minimal()+
    scale_y_continuous(name = "SRAG by Covid/SRAG",
                       sec.axis = sec_axis(~ . * fac_lab,
                                           name = "Number of Cases "))+
    scale_color_manual(values = "Black")+
    scale_fill_manual(values = c(pal_wes[10], pal_wes[1]), 
                      name = element_blank())
  # ggsave(paste0("scripts_R_genericos/Covid_percentage/img/", i,"_percentage_covid_srag.png"),
  #        plot = p.casos.list[[i]],
  #        dpi = 300,
  #        height = 9,
  #        width = 9)
}

library(grid)
library(gridExtra)


grid.plot<-ggarrange(
  p.casos.list$AC+theme(legend.position = "none"),
  p.casos.list$AL+theme(legend.position = "none"),
  p.casos.list$AM+theme(legend.position = "none"),
  p.casos.list$BA+theme(legend.position = "none"),
  p.casos.list$CE+theme(legend.position = "none"),
  p.casos.list$DF+theme(legend.position = "none"),
  p.casos.list$ES+theme(legend.position = "none"),
  p.casos.list$GO+theme(legend.position = "none"),
  p.casos.list$MA+theme(legend.position = "none"),
  p.casos.list$MG+theme(legend.position = "none"),
  p.casos.list$MS+theme(legend.position = "none"),
  p.casos.list$MT+theme(legend.position = "none"),
  p.casos.list$PA+theme(legend.position = "none"),
  p.casos.list$PB+theme(legend.position = "none"),
  p.casos.list$PE+theme(legend.position = "none"),
  p.casos.list$PI+theme(legend.position = "none"),
  p.casos.list$PR+theme(legend.position = "none"),
  p.casos.list$RJ+theme(legend.position = "none"),
  p.casos.list$RN+theme(legend.position = "none"),
  p.casos.list$RS+theme(legend.position = "none"),
  p.casos.list$SC+theme(legend.position = "none"),
  p.casos.list$SE+theme(legend.position = "none"),
  p.casos.list$SP+theme(legend.position = "none"),
  p.casos.list$TO+theme(legend.position = "none"),
  p.casos.list$AP+theme(legend.position = "none"),
  p.casos.list$RO+theme(legend.position = "none"),
  p.casos.list$RR+theme(legend.position = "none"),
  ncol = 3,
  nrow = 9,
  left = "SRAG by Covid/SRAG",
  bottom = "First symptoms Date"
)


#### FAZER GRID COM TODOS OS ESTADOS ###  
