library(plyr)
library(dplyr)
library(ggplot2)
library(zip)
library(aweek)
set_week_start("sunday")

## Em 21/01/2021 commits posteriores a 15/01 parece ter tabelas com linhas faltantes
## Usando o ultimo commit que parece bem
## system("cd ../../../clone_repo_seade_SP/; git checkout 89c35a9")

################################################################################
## SEADE: Tabela com codigos municipios, DRS e populacao
################################################################################
## Leitura do arquivo com os dados de notificacao, que tem os dados dos municipios
not.mun <- read.csv2("../../../clone_repo_seade_SP/data/dados_covid_sp.csv") %>%
    mutate(datahora = as.Date(datahora)) %>%
    filter(datahora < as.Date("2021-01-03"))

## Seleciona as varaveis para usar na tabela de muncipios e retém apenas um resgistro por município
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
    summarise(casos_not = sum(casos_novos),
              obitos_not = sum(obitos_novos)) %>%
    as.data.frame() %>%
    merge(dic.drs) %>%
    mutate(casos_not_pc = 1e5*casos_not/pop, obitos_not_pc = 1e5*obitos_not/pop) %>%
    select(nome_drs:cod_drs,casos_not_pc, obitos_not_pc)

################################################################################
## SEADE: Casos por data de sintoma
################################################################################
## Leitura dos dados
##download.file("https://github.com/seade-R/dados-covid-sp/raw/master/data/casos_obitos_doencas_preexistentes.csv.zip",
##              destfile = "casos_obitos_doencas_preexistentes.csv.zip")

## é preciso um clone do respositorio do SEAD neste path
zip::unzip("../../../clone_repo_seade_SP/data/casos_obitos_doencas_preexistentes.csv.zip", junkpaths = TRUE)
## zip::unzip("casos_obitos_doencas_preexistentes.csv_2021_01_15.zip", junkpaths = TRUE)

casos.all <- read.csv2("casos_obitos_doencas_preexistentes.csv") %>% 
    mutate(data_inicio_sintomas = as.Date(data_inicio_sintomas)) %>%  
    filter(data_inicio_sintomas < as.Date("2021-01-03")) %>% ## para filtrar apenas semanas epidemiológicas de 2020
    mutate(semana_epidem = date2week(data_inicio_sintomas, numeric = TRUE)) 
    

casos.semana <-
    casos.all %>%
    filter(diagnostico_covid19 == "CONFIRMADO") %>% 
    group_by(nome_munic, semana_epidem) %>%
    summarise(casos_novos = n(), obitos_novos = sum(obito ==1)) %>%
    ungroup() %>%
    merge(dic.mun[,c("nome_munic", "nome_drs", "cod_drs")], by = "nome_munic", all.x=TRUE)
    
casos.semana.drs <-
    casos.semana %>%
    group_by(nome_drs, semana_epidem) %>%
    summarise(casos_sin = sum(casos_novos),
              obitos_sin = sum(obitos_novos)) %>%
    ungroup() %>%
    inner_join(dic.drs, by = "nome_drs") %>% 
    mutate(casos_sin_pc = 1e5*casos_sin/pop, obitos_sin_pc = 1e5*obitos_sin/pop) %>%
    inner_join(not.drs, by = c("nome_drs", "semana_epidem")) %>%
    select(nome_drs, semana_epidem, cod_drs.x, pop, pop_60, area, casos_not, casos_sin, casos_not_pc, casos_sin_pc,
           obitos_not, obitos_sin, obitos_not_pc, obitos_sin_pc)

## Total de casos
total.casos.drs <- aggregate( casos.semana.drs[,c(3,4,11,12)], by = list(drs = casos.semana.drs$nome_drs), sum)[, c(1,2,4,3,5)]


## Maximo de casos e obitos e semanas de pico
## Por data sintoma
maximos.sin <- casos.semana.drs %>%
    group_by(nome_drs) %>%
    summarise(tot.casos = sum(casos_not),
              tot.casos.pc = sum(casos_not_pc),
              tot.obitos = sum(obitos_not),
              tot.obitos.pc = sum(obitos_not_pc),
              casos.max = max(casos_sin),
              casos.pc.max = max(casos_sin_pc),
              semana.casos.max = semana_epidem[which.max(casos_sin)],
              obitos.max = max(obitos_sin),
              obitos.pc.max = max(obitos_sin_pc),
              semana.obitos.max = semana_epidem[which.max(obitos_sin)]) %>%
    as.data.frame()

## Por data de notificação
maximos.not <- not.drs %>%
    group_by(nome_drs) %>%
    summarise(tot.casos = sum(casos_not),
              tot.casos.pc = sum(casos_not_pc),
              tot.obitos = sum(obitos_not),
              tot.obitos.pc = sum(obitos_not_pc),
              casos.max = max(casos_not),
              casos.pc.max = max(casos_not_pc),
              semana.casos.max = semana_epidem[which.max(casos_not)],
              obitos.max = max(obitos_not),
              obitos.pc.max = max(obitos_not_pc),
              semana.obitos.max = semana_epidem[which.max(obitos_not)]) %>%
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
## Todos os graficos juntos
png("outputs/casos_por_semana.png", width =900, height = 600)
casos.semana.drs %>%
    ##filter(semana_epidem < max(semana_epidem)-1) %>%
    ##filter(semana_epidem < 42) %>%
    ggplot(aes(semana_epidem, casos_not_pc)) +
    geom_line(aes(col = "Notificação")) +
    geom_line(aes(y=casos_sin_pc, col = "Sintoma")) +
    facet_wrap(~nome_drs, nrow =) +
    xlab("Semana epidemiológica") +
    ylab("Casos por 100 mil habitantes") +
    theme_bw() +
    theme(legend.position = c(0.5, 0.1)) +
    labs(color = "Data de referência")
dev.off()
## Cada DRS em um grafico separado
for(nome in unique(casos.semana.drs$nome_drs)){
    png(paste0("outputs/casos_por_semana_",nome,".png"))
    p1 <-
        casos.semana.drs %>%
        ##filter(semana_epidem < max(semana_epidem) & nome_drs == nome) %>%
        filter(nome_drs == nome) %>%
        ggplot(aes(semana_epidem, casos_not_pc)) +
        geom_line(aes(col = "Notificação"), size = 1.25) +
        geom_line(aes(y=casos_sin_pc, col = "Sintoma"), size = 1.25) +
        xlab("Semana epidemiológica") +
        ylab("Casos por 100 mil habitantes") +
        theme_bw() +
        theme(legend.position = c(0.15, 0.9),
              axis.text=element_text(size=14),
              axis.title=element_text(size=15),
              plot.title = element_text(size=16, face="bold")) +
        labs(color = "Data de referência") +
        scale_color_manual(values = c("darkblue", "darkred")) +
        ggtitle(nome)
    print(p1)
    dev.off()
}

## Obitos
png("outputs/obitos_por_semana.png", width =900, height = 600)
casos.semana.drs %>%
    ##filter(semana_epidem < max(semana_epidem)-1) %>%
    ggplot(aes(semana_epidem, obitos_not_pc)) +
    geom_line(aes(col = "Notificação")) +
    geom_line(aes(y=obitos_sin_pc, col = "Sintoma")) +
    facet_wrap(~nome_drs) +
    xlab("Semana epidemiológica") +
    ylab("Óbitos por 100 mil habitantes") +
    theme_bw() +
    theme(legend.position = c(0.5, 0.1)) +
    labs(color = "Data de referência")
dev.off()
## Um grafico por DRS
for(nome in unique(casos.semana.drs$nome_drs)){
    png(paste0("outputs/obitos_por_semana_",nome,".png"))
    p1 <-
        casos.semana.drs %>%
        ##filter(semana_epidem < max(semana_epidem) & nome_drs == nome) %>%
        filter(nome_drs == nome) %>%
        ggplot(aes(semana_epidem, obitos_not_pc)) +
        geom_line(aes(col = "Notificação"), size = 1.25) +
        geom_line(aes(y=obitos_sin_pc, col = "Sintoma"), size = 1.25) +
        xlab("Semana epidemiológica") +
        ylab("Óbitos por 100 mil habitantes") +
        theme_bw() +
        theme(legend.position = c(0.15, 0.9),
              axis.text=element_text(size=14),
              axis.title=element_text(size=15),
              plot.title = element_text(size=16, face="bold")) +
        labs(color = "Data de referência") +
        scale_color_manual(values = c("darkblue", "darkred")) +
        ggtitle(nome)
    print(p1)
    dev.off()
}

## Notificacoes por dia no estado: contam casos que entraram no dia (argh!), não é data de notificação
n.not.dia <-
    not.mun %>%
    mutate(datahora = as.Date(datahora)) %>%
    group_by(datahora) %>%
    summarise(casos.cum = sum(casos), casos_novos = sum(casos_novos)) %>%
    ungroup
n.not.dia$diff.cum <- c(n.not.dia$casos.cum[1], diff(n.not.dia$casos.cum))

## Aqui vemos o represamento de notificações em 12/nov, o que mostra que o campo "casos_novos"
## registra n de casos novo digitados em cada data
png("casos_novos_dia_seade.png", width = 600)
n.not.dia %>%
    ggplot(aes(datahora, casos_novos)) +
    geom_line() +
    theme_bw()
    ## geom_line(aes(y=diff.cum), col = "red")
dev.off()

## Voltando ao master
## system("cd ../../../clone_repo_seade_SP/; git checkout master")
