library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(zip)
library(aweek)
library(lubridate)
library(rmarkdown)
source("../../nowcasting/fct/get.last.date.R")
set_week_start("Sunday")

## Dicionario de nomes de estados, do InfoGripe
dic.territ <- read.csv("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/base_territorial/DICIONARIO_territorios_id.csv")
dic.estados <- filter(dic.territ, tipo_id ==1 & id != 99)
dic.estados$id.regiao <- as.integer(substring(dic.estados$id, 1, 1))
dic.estados <- merge(dic.estados, data.frame(id.regiao = 1:5, regiao = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")),
                    by= "id.regiao")
################################################################################
## Planilha com nowcasting dos estados do site do Observatorio
################################################################################
dir1 <- "../../site/dados/estado/"
siglas.estado <- dir(dir1)
dir2 <- paste0(dir1,siglas.estado[1],"/tabelas_nowcasting_para_grafico/")
data <- get.last.date(dir2)
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
                         semana = ifelse(data>as.Date("2021-01-02"), semana+53, semana)) ## POG para ter os dois anos epidemiologicos, consertar isso


## Maiores datas em cada estado
observ.data.max <- observ.seman %>%
    group_by(estado) %>%
    summarise(data.max = max(data), sem.max = max(semana)) %>%
    ungroup()

################################################################################
## Dados do Infogripe sem filtros
################################################################################
## Leitura 
infogripe <- read.csv2("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_sintomas.csv")
##infogripe$data.de.publicação <- as.Date(infogripe$data.de.publicação)
## Filtra apenas os dados de casos de srag do estado
infogr.estado <- filter(infogripe, Tipo == "Estado"&
                                   Ano.epidemiológico > 2019 &
                                   ## data.de.publicação == max(data.de.publicação) &
                                   dado == "srag" &
                                   escala == "casos") %>%
    select(UF,
           Unidade.da.Federação,
           Ano.epidemiológico,
           Semana.epidemiológica,
           Casos.semanais.reportados.até.a.última.atualização,
           limite.inferior.da.estimativa,
           casos.estimados,
           limite.superior.da.estimativa) %>%
    merge(dic.estados[, 2:3], by.x = "UF", by.y = "id") %>%
    dplyr::rename(id.estado = UF,
           nome.estado = Unidade.da.Federação,
           semana = Semana.epidemiológica,
           casos.obs = Casos.semanais.reportados.até.a.última.atualização,
           lower = limite.inferior.da.estimativa,
           casos.est = casos.estimados,
           upper = limite.superior.da.estimativa,
           sigla.estado = sigla) %>%
    mutate(semana = ifelse(Ano.epidemiológico == 2021, semana + 53, semana))

## Ultima semana e sua última data no Infogripe
##ultima.sem <- max(infogr.estado$semana)
## ultimo.dia <- max(infogr.estado$data, na.rm=TRUE)
## Adiciona id de ultima semana completa na planilha de datas maximas nos dados do Observatorio
## observ.data.max %<>%
##     mutate(dif.data = data.max -  max(infogr.estado$data),
##            sem.max = ifelse(dif.data < 0, max(infogr.estado$semana) + as.numeric(dif.data)%/%7, max(infogr.estado$semana))) %>%
##     as.data.frame()

################################################################################
## Dados do Infogripe sem filtros
################################################################################
## Leitura 
infogripe2 <- read.csv2("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes.csv")

## Filtra apenas os dados de casos de srag do estado
infogr2.estado <- filter(infogripe2, Tipo == "Estado"&
                                   Ano.epidemiológico > 2019 &
                                   ## data.de.publicação == max(data.de.publicação) &
                                   dado == "srag" &
                                   escala == "casos") %>%
    select(UF,
           Unidade.da.Federação,
           Semana.epidemiológica,
           Ano.epidemiológico,
           Casos.semanais.reportados.até.a.última.atualização,
           limite.inferior.da.estimativa,
           casos.estimados,
           limite.superior.da.estimativa) %>%
    merge(dic.estados[, 2:3], by.x = "UF", by.y = "id") %>%
    dplyr::rename(id.estado = UF,
           nome.estado = Unidade.da.Federação,
           semana = Semana.epidemiológica,
           casos.obs = Casos.semanais.reportados.até.a.última.atualização,
           lower = limite.inferior.da.estimativa,
           casos.est = casos.estimados,
           upper = limite.superior.da.estimativa,
           sigla.estado = sigla) %>%
    mutate(semana = ifelse(Ano.epidemiológico == 2021, semana + 53, semana))

################################################################################
## Dados do Infogripe com filtros de febre
################################################################################
## Leitura 
infogripe3 <- read.csv2("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_febre.csv")

## Filtra apenas os dados de casos de srag do estado
infogr3.estado <- filter(infogripe3, Tipo == "Estado"&
                                   Ano.epidemiológico > 2019 &
                                   ## data.de.publicação == max(data.de.publicação) &
                                   dado == "srag" &
                                   escala == "casos") %>%
    select(UF,
           Unidade.da.Federação,
           Semana.epidemiológica,
           Ano.epidemiológico,
           Casos.semanais.reportados.até.a.última.atualização,
           limite.inferior.da.estimativa,
           casos.estimados,
           limite.superior.da.estimativa) %>%
    merge(dic.estados[, 2:3], by.x = "UF", by.y = "id") %>%
    dplyr::rename(id.estado = UF,
           nome.estado = Unidade.da.Federação,
           semana = Semana.epidemiológica,
           casos.obs = Casos.semanais.reportados.até.a.última.atualização,
           lower = limite.inferior.da.estimativa,
           casos.est = casos.estimados,
           upper = limite.superior.da.estimativa,
           sigla.estado = sigla) %>%
    mutate(semana = ifelse(Ano.epidemiológico == 2021, semana + 53, semana))

################################################################################
## Nowcasting por diferença de bases, por Leo Bastos
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



################################################################################
## Funcoes para graficos
################################################################################


## Funcao para graficos semanais de SRAG: todos os filtros do Infogripe e o nowcasting Observatório
plot.seman <- function(sigla){
    ##max.sem <- observ.data.max$sem.max[observ.data.max$estado==sigla]
    data1  <-  infogr.estado %>% filter(sigla.estado == sigla) 
    ##data2 <- observ.seman %>% filter(semana <= (max.sem) & sigla.estado == sigla)
    data2 <- observ.seman %>% filter(estado == sigla)
    data3  <-  infogr2.estado %>% filter(sigla.estado == sigla)
    data4  <-  infogr3.estado %>% filter(sigla.estado == sigla) 
    p1 <- ggplot(data1, aes(semana)) +
        geom_line(aes(y=casos.obs, color = "InfoGripe sem filtros"), lty =2 ) +
        geom_line(aes(y=casos.est, color = "InfoGripe sem filtros")) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill= "InfoGripe sem filtros"), alpha =0.1) +
        geom_line(data = data3, aes(y=casos.obs, color = "InfoGripe com filtros"), lty =2) +
        geom_line(data = data3, aes(y=casos.est, color = "InfoGripe com filtros")) +
        geom_ribbon(data=data3, aes(ymin = lower, ymax = upper, fill = "InfoGripe com filtros"), alpha =0.1) +
        geom_line(data = data4, aes(y=casos.obs, color = "InfoGripe filtro febre"), lty =2) +
        geom_line(data = data4, aes(y=casos.est, color = "InfoGripe filtro febre")) +
        geom_ribbon(data = data4, aes(ymin = lower, ymax = upper, fill = "InfoGripe filtro febre"), alpha =0.1) +
        geom_line(data = data2, aes(y=n.casos, color= "Observatório"), lty =2 ) +
        geom_line(data = data2, aes(y=estimate, color= "Observatório")) +
        geom_ribbon(data = data2, aes(ymin = lower.merged.pred, ymax = upper.merged.pred, fill= "Observatório"),
                    alpha =0.1) +
        xlab("Semana epidemiológica dos sintomas") +
            ylab("N de casos semanais SRAG") +
        scale_color_manual(name="", values = 5:2) +
        scale_fill_manual(name = "", values = 5:2) +
        ggtitle(sigla) +
        theme_bw() +
        theme(legend.position = c(0.15, 0.8))
    p1
}

## SRAG sem filtros Infogripe, nowcasting por dif de bases e Observatório
plot.seman2 <- function(sigla){
    data1  <-  infogr.estado %>% filter(sigla.estado == sigla) 
    data2 <- observ.seman %>% filter(estado == sigla)
    data3  <-  difsragh %>% filter(sigla.estado == sigla)
    p1 <-
        ggplot(data1, aes(semana)) +
        geom_line(aes(y=casos.obs, color = "InfoGripe sem filtros"), lty =2 ) +
        geom_line(aes(y=casos.est, color = "InfoGripe sem filtros")) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill= "InfoGripe sem filtros"), alpha =0.2) +
        geom_line(data = data3, aes(y=casos.obs, color = "Dif. bases"), lty =2) +
        geom_line(data = data3, aes(y=casos.est, color = "Dif. bases")) +
        geom_ribbon(data=data3, aes(ymin = lower, ymax = upper, fill = "Dif. bases"), alpha =0.2) +
        geom_line(data = data2, aes(y=n.casos, color= "Observatório"), lty =2 ) +
        geom_line(data = data2, aes(y=estimate, color= "Observatório")) +
        geom_ribbon(data = data2, aes(ymin = lower.merged.pred, ymax = upper.merged.pred, fill= "Observatório"),
                    alpha =0.2) +
        xlab("Semana epidemiológica dos sintomas") +
            ylab("N de casos semanais SRAG") +
        scale_color_manual(name="", values = 4:2) +
        scale_fill_manual(name = "", values = 4:2) +
        ggtitle(sigla) +
        theme_bw() +
        theme(legend.position = c(0.15, 0.8))
    p1
}


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
