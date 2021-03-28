library(plyr)
library(dplyr)
library(ggplot2)
library(zip)
library(aweek)
library(lubridate)
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

################################################################################
## SEADE: Casos por data de sintoma
################################################################################
## Leitura dos dados
##download.file("https://github.com/seade-R/dados-covid-sp/raw/master/data/casos_obitos_doencas_preexistentes.csv.zip",
##              destfile = "casos_obitos_doencas_preexistentes.csv.zip")

## é preciso um clone do respositorio do SEAD neste path
zip::unzip("../../../dados/seade/data/casos_obitos_doencas_preexistentes.csv.zip", junkpaths = TRUE)
## zip::unzip("casos_obitos_doencas_preexistentes.csv_2021_01_15.zip", junkpaths = TRUE)

casos.all <- read.csv2("casos_obitos_doencas_preexistentes.csv") %>%
    ##mutate(dt_sint = as.Date(data_inicio_sintomas)) %>%
    mutate(semana = date2week(data_inicio_sintomas, floor_day = TRUE), dt_semana = week2date(semana)+6)

    
casos.semana <-
    casos.all %>%
    filter(diagnostico_covid19 == "CONFIRMADO", !is.na(dt_semana)) %>%
    group_by(nome_munic, dt_semana) %>%
    summarise(casos_novos = n(), obitos_novos = sum(obito ==1)) %>%
    ungroup() %>%
    merge(dic.mun[,c("nome_munic", "nome_drs", "cod_drs")], by = "nome_munic", all.x=TRUE)
    
casos.semana.drs <-
    casos.semana %>%
    group_by(nome_drs, dt_semana) %>%
    summarise(casos_sin = sum(casos_novos),
              obitos_sin = sum(obitos_novos)) %>%
    ungroup() %>%
    inner_join(dic.drs, by = "nome_drs") %>% 
    mutate(casos_sin_pc = 1e5*casos_sin/pop, obitos_sin_pc = 1e5*obitos_sin/pop) %>%
    inner_join(not.drs, by = c("nome_drs", "dt_semana")) %>%
    mutate(semana = date2week(dt_semana, floor_day = TRUE, factor = TRUE)) %>%
    select(nome_drs, dt_semana, semana, cod_drs.x, pop, pop_60, area, casos_not, casos_sin, casos_not_pc, casos_sin_pc,
           obitos_not, obitos_sin, obitos_not_pc, obitos_sin_pc)

## Total de casos
total.casos.drs <- aggregate( casos.semana.drs[,7:10], by = list(drs = casos.semana.drs$nome_drs), sum)[, c(1,2,4,3,5)]


## Maximo de casos e obitos e semanas de pico
## Por data sintoma
maximos.sin <-
    casos.semana.drs %>%
    group_by(nome_drs) %>%
    summarise(tot.casos = sum(casos_not),
              tot.casos.pc = sum(casos_not_pc),
              tot.obitos = sum(obitos_not),
              tot.obitos.pc = sum(obitos_not_pc),
              casos.max = max(casos_sin),
              casos.pc.max = max(casos_sin_pc),
              semana.casos.max = semana[which.max(casos_sin)],
              obitos.max = max(obitos_sin),
              obitos.pc.max = max(obitos_sin_pc),
              semana.obitos.max = semana[which.max(obitos_sin)]) %>%
    as.data.frame()

## Por data de notificação
maximos.not <-
    not.drs %>%
    group_by(nome_drs) %>%
    summarise(tot.casos = sum(casos_not),
              tot.casos.pc = sum(casos_not_pc),
              tot.obitos = sum(obitos_not),
              tot.obitos.pc = sum(obitos_not_pc),
              casos.max = max(casos_not),
              casos.pc.max = max(casos_not_pc),
              semana.casos.max = semana[which.max(casos_not)],
              obitos.max = max(obitos_not),
              obitos.pc.max = max(obitos_not_pc),
              semana.obitos.max = semana[which.max(obitos_not)]) %>%
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
    filter(dt_semana < max(dt_semana)) %>%
    ggplot(aes(dt_semana, casos_not_pc)) +
    geom_line(aes(col = "Notificação")) +
    geom_line(aes(y=casos_sin_pc, col = "Sintoma")) +
    facet_wrap(~nome_drs, nrow =) +
    xlab("Ultimo dia da semana epidemiológica") +
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
        filter(dt_semana < max(dt_semana) & nome_drs == nome) %>%
        ##filter(nome_drs == nome) %>%
        ggplot(aes(dt_semana, casos_not_pc)) +
        geom_line(aes(col = "Notificação"), size = 1.25) +
        geom_line(aes(y=casos_sin_pc, col = "Sintoma"), size = 1.25) +
        xlab("Ultimo dia da semana epidemiológica") +
        ylab("Casos por 100 mil habitantes") +
        theme_bw() +
        theme(legend.position = c(0.15, 0.9),
              axis.text=element_text(size=14),
              axis.title=element_text(size=15),
              plot.title = element_text(size=16, face="bold")) +
        labs(color = "Data de referência") +
        scale_color_manual(values = c("darkblue", "darkred")) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b") +
        ggtitle(nome)
    print(p1)
    dev.off()
}

## Obitos
png("outputs/obitos_por_semana.png", width =900, height = 600)
casos.semana.drs %>%
    filter(dt_semana < max(dt_semana)) %>%
    ggplot(aes(dt_semana, obitos_not_pc)) +
    geom_line(aes(col = "Notificação")) +
    geom_line(aes(y=obitos_sin_pc, col = "Sintoma")) +
    facet_wrap(~nome_drs) +
    xlab("Último dia da semana epidemiológica") +
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
        filter(dt_semana < max(dt_semana) & nome_drs == nome) %>%
        ##filter(nome_drs == nome) %>%
        ggplot(aes(dt_semana, obitos_not_pc)) +
        geom_line(aes(col = "Notificação"), size = 1.25) +
        geom_line(aes(y=obitos_sin_pc, col = "Sintoma"), size = 1.25) +
        xlab("Ùltimo dia da semana epidemiológica") +
        ylab("Óbitos por 100 mil habitantes") +
        theme_bw() +
        theme(legend.position = c(0.15, 0.9),
              axis.text=element_text(size=14),
              axis.title=element_text(size=15),
              plot.title = element_text(size=16, face="bold")) +
        labs(color = "Data de referência") +
        scale_color_manual(values = c("darkblue", "darkred")) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b") +
        ggtitle(nome)
    print(p1)
    dev.off()
}

################################################################################
## SEADE: media movel de internacao em 7 dias
################################################################################
## Leitura e preparação
## Junta Gde São Paulo na DRS 01, tira os dados do municṕio de SP e do Estado
## para ficar apenas com as DRS
dir_dados <- "../../../dados/seade/data/"
interna <-
    read.csv(paste0(dir_dados, "plano_sp_leitos_internacoes.csv"), sep=';', dec=',') %>%
    filter(nome_drs != "Município de São Paulo" & nome_drs != "Estado de São Paulo") %>%
    mutate(datahora = as_date(parse_date_time(datahora, "ymd")),
           week = date2week(datahora, floor_day=TRUE),
           nome_drs2 = ifelse(grepl("Grande SP", nome_drs ), "DRS 01 Grande SP", nome_drs)) %>%
    group_by(datahora, nome_drs2) %>%
    summarise(pacientes_uti_mm7d = sum(pacientes_uti_mm7d),
              total_covid_uti_mm7d = sum(total_covid_uti_mm7d),
              pop = sum(pop),
              internacoes_7d = sum(internacoes_7d)) %>%
    ungroup() %>%
    mutate(ocupacao_leitos = 100 * pacientes_uti_mm7d / total_covid_uti_mm7d,
           leitos_pc = 1e5*total_covid_uti_mm7d / pop,
           internacoes_7d_pc = 1e5*internacoes_7d / pop)

## valores máximos e respectivos dias e semanas
interna.max.min <-
    interna %>%
    group_by(nome_drs2) %>%
    summarise(
        pacientes_uti_mm7d.min = min(pacientes_uti_mm7d),
        pacientes_uti_mm7d.max = max(pacientes_uti_mm7d),
        total_covid_uti_mm7d.min = min(total_covid_uti_mm7d),
        total_covid_uti_mm7d.max = max(total_covid_uti_mm7d),
        leitos_pc.min = min(leitos_pc),
        leitos_pc.max = max(leitos_pc),
        ocupacao_leitos.min = min(ocupacao_leitos),
        ocupacao_leitos.max = max(ocupacao_leitos),
        internacoes_7d.min = min(internacoes_7d),
        internacoes_7d.max = max(internacoes_7d),
        internacoes_7d_pc.min = min(internacoes_7d_pc),
        internacoes_7d_pc.max = max(internacoes_7d_pc),
        dia.pacientes_uti_mm7d.min = datahora[which.min(pacientes_uti_mm7d)],
        dia.pacientes_uti_mm7d.max = datahora[which.max(pacientes_uti_mm7d)],
        dia.leitos_pc.min = datahora[which.min(leitos_pc)],
        dia.leitos_pc.max = datahora[which.max(leitos_pc)],
        dia.ocupacao_leitos.min = datahora[which.min(ocupacao_leitos)],
        dia.ocupacao_leitos.max = datahora[which.max(ocupacao_leitos)],
        dia.internacoes_7d_pc.min = datahora[which.min(internacoes_7d_pc)],
        dia.internacoes_7d_pc.max = datahora[which.max(internacoes_7d_pc)])
        
## Exporta planilha
write.csv(interna.max.min, file = "outputs/maximos_minimos_internacoes.csv")

## Graficos
## Ocupação
for(nome in unique(interna$nome_drs2)){
    png(paste0("outputs/ocupacao_UTI_",nome,".png"))
    p1 <-
        interna %>%
        filter(nome_drs2 == nome) %>%
        ##filter(nome_drs == nome) %>%
        ggplot(aes(datahora, ocupacao_leitos)) +
        geom_line() +
        xlab("Dia") +
        ylab("Ocupação leitos UTI COVID %") +
        theme_bw() +
        theme(
              axis.text=element_text(size=14),
              axis.title=element_text(size=15),
              plot.title = element_text(size=16, face="bold")) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b") +
        ylim(c(0,1)) +
        ggtitle(nome)
    print(p1)
    dev.off()
}

## Media movel de casos internados por 100 mil habitantes
for(nome in unique(interna$nome_drs2)){
    png(paste0("outputs/leitos_UTI_100_mil_hab_",nome,".png"))
    p1 <-
        interna %>%
        filter(nome_drs2 == nome) %>%
        ##filter(nome_drs == nome) %>%
        ggplot(aes(datahora, leitos_pc)) +
        geom_line() +
        xlab("Dia") +
        ylab("Leitos UTI COVID / 100 mil hab") +
        theme_bw() +
        theme(
              axis.text=element_text(size=14),
              axis.title=element_text(size=15),
              plot.title = element_text(size=16, face="bold")) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b") +
        ylim(range(interna$leitos_pc)) +
        ggtitle(nome)
    print(p1)
    dev.off()
}

## Media móvel de internações por 100 mil habitantes
for(nome in unique(interna$nome_drs2)){
    png(paste0("outputs/media_movel_7d_internacoes_100_mil_hab",nome,".png"))
    p1 <-
        interna %>%
        filter(nome_drs2 == nome) %>%
        ##filter(nome_drs == nome) %>%
        ggplot(aes(datahora, internacoes_7d_pc)) +
        geom_line() +
        xlab("Dia") +
        ylab("Novas internações / 100 mil hab (Média 7 dias)") +
        theme_bw() +
        theme(
              axis.text=element_text(size=14),
              axis.title=element_text(size=15),
              plot.title = element_text(size=16, face="bold")) +
        scale_x_date(date_breaks = "2 months", date_labels = "%b") +
        ylim(range(interna$internacoes_7d_pc)) +
        ggtitle(nome)
    print(p1)
    dev.off()
}


################################################################################
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
## system("cd ../../../dados/seade; git checkout master")
