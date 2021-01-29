library(dplyr)
library(plyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(readr)
library(lubridate)
library(NobBS)
library(aweek)
source("../../nowcasting/fct/gera.nowcasting.R")
source("../../nowcasting/fct/write.notificacoes.data.R")
source("../../nowcasting/fct/prepara_dados2.R")
source("../../nowcasting/fct/get.last.date.R")
source("../../nowcasting/fct/read.sivep.R")
source("../../nowcasting/fct/preenche.now.R")


## Leitura dos dados mais recentes: sivep residentes, maiores de 15 anos
data.dir <- "../../dados/SIVEP-Gripe/"
raw.data <- read.sivep(dir = data.dir, escala = "estado",
geocode = 13, sigla = "AM", data = get.last.date(data.dir))

## Plots
p1 <-
    raw.data %>%
    dplyr::filter(!is.na(dt_interna)&
           hospital==1 &
           dt_interna>as.Date("2019-12-31")&
           dt_interna<Sys.Date()) %>%
    mutate(sem_inter = week2date(date2week(dt_interna, floor_day=TRUE))) %>%
    group_by(sem_inter, sg_uf_not) %>%
    dplyr::summarise(N = n()) %>%
    ggplot(aes(sem_inter, N)) +
    geom_line() +
    theme_bw()
png("internacoe_srag_residentes_AM_por_estado_not.png", width =600)
p1 + facet_wrap(~sg_uf_not, scales = "free_y")
dev.off()

png("internacoe_srag_residentes_AM_notif_SP.png", width =600)
raw.data %>%
    filter(!is.na(dt_interna)&
           hospital==1 &
           dt_interna>as.Date("2019-12-31")&
           dt_interna<Sys.Date() &
           sg_uf_not=="SP") %>%
    mutate(sem_inter = week2date(date2week(dt_interna, floor_day=TRUE))) %>%
    group_by(sem_inter, sg_uf_not) %>%
    dplyr::summarise(N = n()) %>%
    ggplot(aes(sem_inter, N)) +
    geom_point() +
    geom_line(lty=2) +
    theme_bw() +
    ggtitle("SRAG residentes em AM notificados em SP")
dev.off()

## Tabela CNES: deixei 
cnes <- read.csv2("tbEstabelecimento202012.csv")

## Internados em SP desde dez/2019
raw.data %>%
    filter(dt_interna>as.Date("2020-11-30")&
           !is.na(dt_interna)&
           hospital==1 &
           dt_interna<Sys.Date()&
           sg_uf_not=="SP") %>%
    group_by(co_uni_not) %>%
    dplyr::summarise(N=n()) %>%
    ungroup() %>%
    merge(cnes, all.x=TRUE, by.x = c("co_uni_not"), by.y="CO_CNES") %>%
    arrange(-N)

## Recorte da SIVEP com estes casos e linkada com CNES
internacoes.AM.SP <-
    raw.data %>%
    filter(dt_interna>as.Date("2020-11-30")&
           !is.na(dt_interna)&
           hospital==1 &
           dt_interna<Sys.Date()&
           sg_uf_not=="SP") %>%
    merge(cnes, all.x=TRUE, by.x = c("co_uni_not"), by.y="CO_CNES")
    
write.csv(internacoes.AM.SP, "sivep_residentes_AM_hospitalizados_SP_linkage_CNES.csv")
