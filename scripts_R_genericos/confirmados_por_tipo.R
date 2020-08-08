library(effects)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(viridis)
library(bbmle)
source("../nowcasting/fct/get.last.date.R")
source("../nowcasting/fct/read.sivep.R")

data.dir <- "../dados/SIVEP-Gripe/"
agora <- get.last.date(data.dir)
antes <- "2020_07_21"
dados <- read.sivep(dir = data.dir, escala = "pais", data = agora)
dados2 <- read.sivep(dir = data.dir, escala = "pais", data = antes)

dados.crit  <-
    dados %>%
    filter(classi_fin == 5) %>%
    mutate(criterio = ifelse(is.na(criterio), "NI", criterio)) %>%
    group_by(dt_sin_pri, criterio) %>%
    summarise( N= n(), base = agora)
dados2.crit  <-
    dados2 %>%
    filter(classi_fin == 5) %>%
    mutate(criterio = ifelse(is.na(criterio), "NI", criterio)) %>%
    group_by(dt_sin_pri, criterio) %>%
    summarise( N= n(), base = antes)

criterios <- list(
    "1" = "Laboratorial",
    "2" = "Clínico-Epidemiológico",
    "3" = "Clínico",
    "4" = "Clínico Imagem",
    "NI" = "Não informado"
)
criterio.lab <- function(variable, value)
    return(criterios[value])

png("casos_conf_por_crit.png", width =900)
rbind(dados.crit, dados2.crit) %>%
    ggplot(aes(dt_sin_pri, N, group = base)) +
    geom_line(aes(color=base)) +
    facet_wrap(~criterio, scales = "free", labeller = criterio.lab) +
    theme_bw()
dev.off()
    

