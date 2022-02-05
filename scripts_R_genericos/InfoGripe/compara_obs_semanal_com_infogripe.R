library(plyr)
library(dplyr)
library(vroom)
library(magrittr)
library(ggplot2)
library(zip)
library(aweek)
library(lubridate)
library(rmarkdown)
source("../../nowcasting/fct/get.last.date.R")
source("functions.R")
set_week_start("Sunday")

## check if this is being run automatically to auto-update
AUTO <- FALSE
if (sys.nframe() == 0L) {
    args = commandArgs(trailingOnly=TRUE)
    if (length(args) > 1 && args[1] == "--auto") {
        last_update <- args[2]
        AUTO <- TRUE
        print(paste('data de atualização:', last_update))
    }
}

## Dicionario de nomes de estados, do InfoGripe
##dic.territ <- read.csv("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/base_territorial/DICIONARIO_territorios_id.csv")
dic.territ <- read.csv("https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/base_territorial/DICIONARIO_territorios_id.csv")
dic.estados <- filter(dic.territ, tipo_id ==1 & id != 99)
dic.estados$id.regiao <- as.integer(substring(dic.estados$id, 1, 1))
dic.estados <- merge(dic.estados, data.frame(id.regiao = 1:5, regiao = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")),
                    by= "id.regiao")
################################################################################
## Planilha com nowcasting dos estados do site do Observatorio
################################################################################
dir1 <- "../../site/dados/estado/"
siglas.estado <- dir(dir1)
dir2 <- paste0(dir1, "DF", "/tabelas_nowcasting_para_grafico/")
data <- get.last.date(dir2)
print(paste('data nowcasting Obs:', data))

# last report is more recent than last data
if (AUTO && last_update > data)
    quit(save = "no", status = 1)

tmp <- read.csv(paste0(dir2,"nowcasting_diario_srag_",data,".csv"))
tmp$estado <- siglas.estado[1]
observ.now <- tmp[,c(8,1:7)]
for(i in 2:length(siglas.estado)){
    dir2 <- paste0(dir1,siglas.estado[i],"/tabelas_nowcasting_para_grafico/")
    data <- get.last.date(dir2)
    tmp <- read.csv(paste0(dir2,"nowcasting_diario_srag_",data,".csv"))
    tmp$estado <- siglas.estado[i]
    observ.now <- rbind(observ.now, tmp[,c(8,1:7)])
}
## N de casos observados até o fim da serie
siglas.estado <- dir(dir1)
dir2 <- paste0(dir1,siglas.estado[1],"/tabelas_nowcasting_para_grafico/")
data <- get.last.date(dir2)
tmp <- read.csv(paste0(dir2,"nowcasting_acumulado_srag_",data,".csv"))[,c(1,6)]
tmp <- tmp[!is.na(tmp$not.mean.c),]
tmp$n.casos <- c(tmp$not.mean.c[1], diff(tmp$not.mean.c))
tmp$estado <- siglas.estado[1]
observ.ncasos <- tmp[,c(1,3,4)]
for(i in 2:length(siglas.estado)){
    dir2 <- paste0(dir1,siglas.estado[i],"/tabelas_nowcasting_para_grafico/")
    data <- get.last.date(dir2)
    tmp <- read.csv(paste0(dir2,"nowcasting_acumulado_srag_",data,".csv"))[,c(1,6)]
    tmp <- tmp[!is.na(tmp$not.mean.c),]
    tmp$n.casos <- c(tmp$not.mean.c[1], diff(tmp$not.mean.c))
    tmp$estado <- siglas.estado[i]
    observ.ncasos <- rbind(observ.ncasos, tmp[,c(1,3,4)])
}
## Junta os dois objetos
observ.seman <- merge(observ.now[, -3], observ.ncasos, by = c("estado", "data"), all=TRUE) 
## Adiciona semana epidemiologica
observ.seman %<>% mutate(data = as.Date(data)-1,
                         semana = date2week(data, numeric = TRUE),
                         semana = ifelse(data>as.Date("2021-01-02"), semana+53, semana),
                         semana = ifelse(data>as.Date("2022-01-01"), semana+52, semana)) ## POG para ter os tres anos epidemiologicos, consertar isso


## Maiores datas em cada estado
observ.data.max <- observ.seman %>%
    group_by(estado) %>%
    summarise(data.max = max(data), sem.max = max(semana)) %>%
    ungroup()

################################################################################
## Dados do Infogripe sem filtros
################################################################################
## Leitura 
## infogripe <- read.csv2("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_sintomas.csv")
infogripe <- vroom("https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_sintomas.csv.gz",  delim=";" , locale=locale(decimal_mark=',')) %>%
    as.data.frame()

##infogripe$data.de.publicação <- as.Date(infogripe$data.de.publicação)
infogr.last.date <- format(as.Date(max(infogripe[,"data de publicação"])), format = "%Y_%m_%d")
print(paste('data nowcasting InfoGripe:', infogr.last.date))
# last report is more recent than latest data
if (AUTO && last_update > infogr.last.date)
    quit(save = "no", status = 1)

## Filtra apenas os dados de casos de srag do estado e preparar o dataframe
infogr.estado <- info.estado(infogripe)

## Ultima semana e sua última data no Infogripe
##ultima.sem <- max(infogr.estado$semana)
## ultimo.dia <- max(infogr.estado$data, na.rm=TRUE)
## Adiciona id de ultima semana completa na planilha de datas maximas nos dados do Observatorio
## observ.data.max %<>%
##     mutate(dif.data = data.max -  max(infogr.estado$data),
##            sem.max = ifelse(dif.data < 0, max(infogr.estado$semana) + as.numeric(dif.data)%/%7, max(infogr.estado$semana))) %>%
##     as.data.frame()

################################################################################
## Dados do Infogripe com filtros
################################################################################
## Leitura 
## infogripe2 <- read.csv2("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes.csv")

infogripe2 <- vroom("https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes.csv.gz", delim=";", locale=locale(decimal_mark=',')) %>%
    as.data.frame()

## Filtra apenas os dados de casos de srag do estado
infogr2.estado <- info.estado(infogripe2)


################################################################################
## Dados do Infogripe sem filtros de febre
################################################################################
## Leitura 
##infogripe3 <- read.csv2("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_febre.csv")

infogripe3 <- vroom("https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_febre.csv.gz", delim=";", locale=locale(decimal_mark=',')) %>%
    as.data.frame()

## Filtra apenas os dados de casos de srag do estado
infogr3.estado <- info.estado(infogripe3)

################################################################################
## Nowcasting por diferença de bases, por Leo Bastos
## PI: não tá funfando
################################################################################
difsragh <- read.csv("https://gitlab.procc.fiocruz.br/lsbastos/nowcasting_data/-/raw/master/csvs/tbl.UF.h.srag.csv") %>%
    filter(Estado!="Brasil") %>%
    mutate(semana = date2week(dt_event, numeric = TRUE),
           semana = ifelse ( year(dt_event) == 2021,  semana + 53 , semana)) %>%
    dplyr::rename(
               casos.obs = n,
               lower = LI,
               casos.est = Median,
               upper = LS,
               sigla.estado = Estado)

difsragh.last.date <- format(as.Date(max(difsragh$dt_event)), format = "%Y_%m_%d")
print(paste('data nowcasting diff bases:', difsragh.last.date))
# last report is more recent than latest data
if (AUTO && last_update > difsragh.last.date)
    quit(save = "no", status = 1)



################################################################################
## Gera o relatorio
################################################################################
sink(file = "relat_seman_body.Rmd")
for(regiao in unique(dic.estados$regiao)){
    cat("# Região", regiao, "\n \n")
    for(sigla in sort(dic.estados$sigla[dic.estados$regiao == regiao])){
        cat("##", dic.estados$nome[dic.estados$sigla == sigla], "\n \n")
        cat("```{r } \n plot.seman2(\"",sigla,"\") \n``` \n \n", sep="")
    }
}
sink()

save.image()

render("relatorio_InfogripeXObservatorio_nowcastings_semanais.Rmd", output_dir="output")

# superfluo?!
if (AUTO)
    quit(save = "no", status = 0)
