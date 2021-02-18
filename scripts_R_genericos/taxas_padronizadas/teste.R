library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(zoo)
library(lubridate)
library(aweek)
source("../../nowcasting/fct/read.sivep.R")
source("../../nowcasting/fct/get.last.date.R")

## Municipio de São Paulo
## Leitura dos dados mais recentes: sivep residentes, maiores de 15 anos
data.dir <- "../../dados/SIVEP-Gripe/"
raw.data <- read.sivep(dir = data.dir, escala = "municipio",
                       geocode = 3550308, data = get.last.date(data.dir))
raw.data %>%
    mutate(sem_sin_pri = week2date(date2week(dt_sin_pri, floor=TRUE))) %>%
    group_by(sem_sin_pri) %>%
    summarise(N=n()) %>%
    ggplot(aes(sem_sin_pri, N)) +
    geom_point() +
    theme_bw()
    
raw.data %>%
    filter(dt_sin_pri < as.Data("2021-01-04")) %>%
    mutate(sem_sin_pri = week2date(date2week(dt_sin_pri, floor=TRUE)),
           raca = recode(replace_na(cs_raca, "Ignorada"), "1" = "Branca", "2" = "Preta/parda", "3" = "Amarela",
                         "4" = "Preta/parda", "5" = "Indígena", "9" = "Ignorada",)) %>%
    group_by(sem_sin_pri, raca) %>%
    summarise(N=n()) %>%
    mutate(prop = N/sum(N)) %>%
    ggplot(aes(sem_sin_pri, prop)) +
    geom_point() +
    theme_bw() +
    facet_wrap(~raca, scales="free_y")

raw.data %>%
    filter(dt_sin_pri < as.Date("2021-01-04")) %>%
    filter(cs_raca !="9" & !is.na(cs_raca))%>%
    mutate(sem_sin_pri = week2date(date2week(dt_sin_pri, floor=TRUE)),
           raca = recode(cs_raca, "1" = "Branca", "2" = "Preta/parda", "3" = "Amarela",
                         "4" = "Preta/parda", "5" = "Indígena")) %>%
    group_by(sem_sin_pri, raca) %>%
    summarise(N=n()) %>%
    mutate(prop = N/sum(N)) %>%
    ggplot(aes(sem_sin_pri, prop)) +
    geom_point() +
    theme_bw() +
    facet_wrap(~raca, scales="free_y")


raw.data %>%
    filter(dt_sin_pri < as.Date("2021-01-04")&dt_sin_pri > as.Date("2019-12-31")&!is.na(dt_sin_pri)) %>%
    filter(cs_raca !="9" & !is.na(cs_raca))%>%
    mutate(raca = recode(cs_raca, "1" = "Branca", "2" = "Preta/parda", "3" = "Amarela",
                         "4" = "Preta/parda", "5" = "Indígena"),
           periodo = cut(dt_sin_pri,
                         breaks = as.Date(c("2019-12-31", "2020-03-31", "2020-05-01", "2020-10-01", "2021-01-04")),
                                          labels=c("Jan-Apr","Apr-Mai","Mai-out", "Out-Dez"))) %>%
    group_by(periodo,raca) %>%
    summarise(N=n()) %>%
    mutate(prop = N/sum(N)) %>%
    ggplot(aes(periodo, prop)) +
    geom_col(aes(fill=raca)) +
    theme_bw() 


## Idade
raw.data %>%
    filter(dt_sin_pri < as.Date("2021-01-04")) %>%
    mutate(sem_sin_pri = week2date(date2week(dt_sin_pri, floor=TRUE)),
           idade=as.integer(nu_idade_n),
           idade = ifelse(tp_idade!="3", 0, idade))%>%
    filter(idade<110) %>%
    ggplot() +
    geom_boxplot(aes(x= sem_sin_pri, group=sem_sin_pri, y=idade)) +
    theme_bw() 
