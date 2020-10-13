library(plyr)
library(dplyr)
library(ggplot2)
library(zip)
library(aweek)

################################################################################
## SEADE: Tabela com codigos municipios, DRS e populacao
################################################################################
## Leitura do arquivo com os dados de notificacao, que tem os dados dos municipios
not.mun <- read.csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv")
## Seleciona as varaveis para usar na tabela de muncipios e retém apenas um resgistor por município
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
    group_by(nome_drs, semana_epidem) %>%
    summarise(casos_novos = sum(casos_novos),
              obitos_novos = sum(obitos_novos)) %>%
    as.data.frame() %>%
    merge(drs) %>%
    mutate(casos_pc = 1e5*casos_novos/pop, obitos_pc = 1e5*obitos_novos/pop) %>%
    select(nome_drs:cod_drs,casos_pc, obitos_pc)

################################################################################
## SEADE: Casos por data de sintoma
################################################################################
## Leitura dos dados
download.file("https://github.com/seade-R/dados-covid-sp/raw/master/data/casos_obitos_doencas_preexistentes.csv.zip",
              destfile = "casos_obitos_doencas_preexistentes.csv.zip")
zip::unzip("casos_obitos_doencas_preexistentes.csv.zip", junkpaths = TRUE)
casos.all <- read.csv2("casos_obitos_doencas_preexistentes.csv") %>%
    mutate(data_inicio_sintomas = as.Date(data_inicio_sintomas, "%m/%d/%Y"),
           semana_epidem = date2week(data_inicio_sintomas, numeric = TRUE))

casos.semana <-
    casos.all %>%
    filter(diagnostico_covid19 == "CONFIRMADO") %>% 
    group_by(nome_munic, semana_epidem) %>%
    summarise(casos_novos = n(), obitos_novos = sum(obito ==1)) %>%
    ungroup() %>%
    inner_join(dic.mun[,c("nome_munic", "nome_drs", "cod_drs")], by = "nome_munic")
    
casos.semana.drs <-
    casos.semana %>%
    group_by(nome_drs, semana_epidem) %>%
    summarise(casos_sin = sum(casos_novos),
              obitos_sin = sum(obitos_novos)) %>%
    ungroup() %>%
    inner_join(drs, by = "nome_drs") %>% 
    mutate(casos_sin_pc = 1e5*casos_sin/pop, obitos_sin_pc = 1e5*obitos_sin/pop) %>%
    inner_join(not.drs, by = c("nome_drs", "semana_epidem"))    

## Total de casos
total.casos.drs <- aggregate( casos.semana.drs[,c(3,4,11,12)], by = list(drs = casos.semana.drs$nome_drs), sum)[, c(1,2,4,3,5)]
## diferenca total
with(total.casos.drs, sum(casos_novos, na.rm=TRUE) - sum(casos_sin, na.rm=TRUE))

## Maximo de casos e obitos e semanas de pico
## Por data sintoma
maximos.sin <- casos.semana.drs %>%
    group_by(nome_drs) %>%
    summarise(casos.max = max(casos_sin),
              casos.pc.max = max(casos_sin_pc),
              semana.casos.max = semana_epidem[which.max(casos_sin)],
              obitos.max = max(obitos_sin),
              obitos.pc.max = max(obitos_sin_pc),
              semana.obitos.max = semana_epidem[which.max(obitos_sin)]) %>%
    as.data.frame()

## Por data de notificação
maximos.not <- not.drs %>%
    group_by(nome_drs) %>%
    summarise(tot.casos = sum(casos_novos),
              tot.casos.pc = sum(casos_pc),
              tot.obitos = sum(obitos_novos),
              tot.obitos.pc = sum(obitos_pc),
              casos.max = max(casos_novos),
              casos.pc.max = max(casos_pc),
              semana.casos.max = semana_epidem[which.max(casos_novos)],
              obitos.max = max(obitos_novos),
              obitos.pc.max = max(obitos_pc),
              semana.obitos.max = semana_epidem[which.max(obitos_novos)]) %>%
    as.data.frame()

## Exporta planilhas
write.csv(casos.semana.drs, file = "outputs/casos_e_incidencias_DRS_semana_de_notificacao_e_sintomas.csv",
           row.names = FALSE, quote=FALSE)
write.csv(maximos.not , file = "outputs/totais_maximos_e_semana_pico_semana_notificacao.csv", row.names = FALSE, quote=FALSE)
write.csv(maximos.sin , file = "outputs/totais_maximos_e_semana_pico_semana_sintoma.csv", row.names = FALSE, quote=FALSE)

################################################################################
## Graficos ##
################################################################################
## Casos por 100k habitantes
png("outputs/casos_por_semana.png", width =900, height = 600)
casos.semana.drs %>%
    filter(semana_epidem < 41) %>%
    ggplot(aes(semana_epidem, casos_pc)) +
    geom_line(aes(col = "Notificação")) +
    geom_line(aes(y=casos_sin_pc, col = "Sintoma")) +
    facet_wrap(~nome_drs, nrow =) +
    xlab("Semana epidemiológica") +
    ylab("Casos por 100 mil habitantes") +
    theme_bw() +
    theme(legend.position = c(0.5, 0.1)) +
    labs(color = "Data de referência")
dev.off()

## Obitos
png("outputs/obitos_por_semana.png", width =900, height = 600)
casos.semana.drs %>%
    filter(semana_epidem < 41) %>%
    ggplot(aes(semana_epidem, obitos_pc)) +
    geom_line(aes(col = "Notificação")) +
    geom_line(aes(y=obitos_sin_pc, col = "Sintoma")) +
    facet_wrap(~nome_drs) +
    xlab("Semana epidemiológica") +
    ylab("Óbitos por 100 mil habitantes") +
    theme_bw() +
    theme(legend.position = c(0.5, 0.1)) +
    labs(color = "Data de referência")
dev.off()
