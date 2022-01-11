library(tidyverse)
library(zip)
library(aweek)
library(lubridate)
library(vroom)
library(magrittr)
set_week_start("sunday")

## Em 21/01/2021 commits posteriores a 15/01 parece ter tabelas com linhas faltantes
## Usando o ultimo commit que parece bem
## system("cd ../../../dados/seade/; git checkout 89c35a9")
system("cd ../../../dados/seade/; git pull")

################################################################################
## SEADE: Tabela com codigos municipios, DRS e populacao
################################################################################
## Leitura do arquivo com os dados de notificacao, que tem os dados dos municipios
not.mun <- read.csv2("../../../dados/seade/data/dados_covid_sp.csv") %>%
    mutate(datahora = as.Date(datahora))
#%>% filter(datahora < as.Date("2021-01-03"))
################################################################################
## SEADE: Tabela com codigos municipios, DRS e populacao
################################################################################
## Leitura do arquivo com os dados de notificacao, que tem os dados dos municipios
not.mun <- read.csv2("../../../dados/seade/data/dados_covid_sp.csv") %>%
    mutate(datahora = as.Date(datahora))
#%>% filter(datahora < as.Date("2021-01-03"))

## Seleciona as varaveis para usar na tabela de muncipios e retém apenas um registro por município
dic.mun <-
    not.mun %>%
    select(nome_munic,codigo_ibge,nome_ra,cod_ra,nome_drs,cod_drs,pop,pop_60,area,latitude,longitude) %>%
    filter(nome_munic != "Ignorado") %>%
    distinct()
## Tabela das DRS
dic.drs <-
    dic.mun %>%
    group_by(nome_drs, cod_drs) %>%
    summarise(pop = sum(pop), pop_60 = sum(pop_60), area = sum(area))


################################################################################
## SEADE: Casos por data de notificação
################################################################################
## Casos por DRS e semana epidemiológica
not.drs <-
    not.mun %>%
    mutate(semana = date2week(datahora, floor_day=TRUE), dt_semana = week2date(semana)+6) %>%
    ##mutate(semana = ifelse(datahora > as.Date("2021-01-02"), semana_epidem + 53, semana_epidem)) %>%
    group_by(nome_drs, dt_semana) %>%
    summarise(casos_not = sum(casos_novos),
              obitos_not = sum(obitos_novos)) %>%
    as.data.frame() %>%
    merge(dic.drs) %>%
    mutate(casos_not_pc = 1e5*casos_not/pop, obitos_not_pc = 1e5*obitos_not/pop,
           semana = date2week(dt_semana, floor_day=TRUE, factor=TRUE)) %>%
    select(nome_drs:cod_drs,semana, casos_not_pc, obitos_not_pc)

## Casos por data por DRS
not.dia.drs <-
    not.mun %>%
    ##mutate(semana = ifelse(datahora > as.Date("2021-01-02"), semana_epidem + 53, semana_epidem)) %>%
    dplyr::group_by(nome_drs, datahora) %>%
    dplyr::summarise(casos_not = sum(casos_novos),
              obitos_not = sum(obitos_novos)) %>%
    as.data.frame() %>%
    merge(dic.drs) %>% 
    select(nome_drs, datahora, casos_not, obitos_not)

write.csv(not.dia.drs, "casos_obitos_data_notif_seade.csv", row.names=TRUE)

## Total de casos de 1 a 6/01/20222
not.dia.drs %>%
    filter(datahora >= ymd("2022-01-01") & datahora <= ymd("2022-01-06")) %>%
    summarise(caso = sum(casos_not), obitos = sum(obitos_not))

################################################################################
## SEADE: Casos por data de sintoma
################################################################################
## Leitura dos dados
##download.file("https://github.com/seade-R/dados-covid-sp/raw/master/data/casos_obitos_doencas_preexistentes.csv.zip",
##              destfile = "casos_obitos_doencas_preexistentes.csv.zip")

## é preciso um clone do respositorio do SEAD neste path
zip::unzip( "../../../dados/seade/data/casos_obitos_doencas_preexistentes.csv.zip", junkpaths = TRUE)
## zip::unzip("casos_obitos_doencas_preexistentes.csv_2021_01_15.zip", junkpaths = TRUE)

casos.all <- vroom("../../../dados/seade/data/casos_obitos_doencas_preexistentes.csv.zip")

casos.dt.sint <-
    casos.all %>%
    dplyr::group_by(nome_munic, data_inicio_sintomas) %>%
    dplyr::summarise(n.casos = n(), n.obitos = sum(obito)) %>%
    merge(select(dic.mun, nome_munic, cod_drs, nome_drs))

write.csv(casos.dt.sint, "casos_data_sintomas_seade.csv", row.names=FALSE)

## N de casos com inicio de sintoma entre 1 e 6/jan/22
casos.dt.sint %>%
    filter(data_inicio_sintomas>= ymd("2022-01-01") & data_inicio_sintomas<= ymd("2022-01-06")) %>%
    summarise(casos = sum(n.casos), obitos = sum(n.obitos))
## Total de casos acumulados
not.dia.drs %>%
    summarise(caso = sum(casos_not), obitos = sum(obitos_not))

casos.dt.sint %>%
    summarise(casos = sum(n.casos), obitos = sum(n.obitos))
