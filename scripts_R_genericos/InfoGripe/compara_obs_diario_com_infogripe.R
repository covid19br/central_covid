library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(zip)
library(aweek)
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
observ <- tmp[,c(8,1:7)]
for(i in 2:length(siglas.estado)){
    dir2 <- paste0(dir1,siglas.estado[i],"/tabelas_nowcasting_para_grafico/")
    data <- get.last.date(dir2)
    tmp <- read.csv(paste0(dir2,"nowcasting_diario_srag_",data,".csv"))
    tmp$estado <- siglas.estado[i]
    observ <- rbind(observ, tmp[,c(8,1:7)])
}
observ$data <- as.Date(observ$data)
## Maiores datas em cada estado
observ.data.max <- observ %>%
    group_by(estado) %>%
    summarise(data.max = max(data)) %>%
    ungroup()
    
## ## Agrega por semana
observ.seman <-
    observ %>%
    mutate(semana = date2week(data, numeric = TRUE)) %>%
    group_by(estado, semana) %>%
    summarise(casos.obs = sum(estimate.merged),
              casos.est = sum(estimate),
              lower = sum(lower.merged.pred),
              upper = sum(upper.merged.pred)) %>%
    ungroup() %>%
    ##mutate(casos.obs = ifelse(is.na(lower), casos.obs, NA)) %>%
    merge(dic.estados[, c("id","sigla","nome")], by.x = "estado", by.y = "sigla") %>%
    mutate(sigla.estado = estado, id.estado = id, nome.estado = nome, base = "Observatório") %>%
    select(id.estado, sigla.estado, nome.estado, semana, casos.obs, casos.est, lower, upper, base) %>%  
    as.data.frame()

################################################################################
## Dados do Infogripe
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
           sigla.estado = sigla)

## Transforma semana epidemiol em data do ultimo dia dela e divide n de casos por 7
## para plotar na escala diária
infogr.estado %<>%
    mutate(sem=ifelse(nchar(semana)==1, paste0("0",semana), as.character(semana)),
                       data = week2date(as.aweek(paste0("2020-W",sem,"-7"))),
           m.casos.obs = casos.obs/7, m.lower = lower/7, m.casos.est = casos.est/7, m.upper = upper/7)
## Ultima semana e sua última data no Infogripe
ultima.sem <- max(infogr.estado$semana)
ultimo.dia <- max(infogr.estado$data)
## Adiciona id de ultima semana completa na planilha de datas maximas nos dados do Observatorio
observ.data.max %<>%
    mutate(dif.data = data.max -  max(infogr.estado$data),
           sem.max = ifelse(dif.data < 0, max(infogr.estado$semana) + as.numeric(dif.data)%/%7, max(infogr.estado$semana))) %>%
    as.data.frame()

################################################################################
## Funcoes para graficos
################################################################################

## Funcao para Graficos diarios
plot.diario <- function(sigla){
    data1  <- fortify(infogr.estado) %>% filter(sigla.estado == sigla) 
    data2 <- fortify(observ) %>% filter(estado == sigla) # & !is.na(estimate))
    ## plot
    p1 <- ggplot(data1, aes(x=data)) +
        geom_line(aes(y=m.casos.obs), lty = 2, col ="red") +
        geom_line(aes(y=m.casos.est), col = "red") +
        geom_ribbon(aes(ymin = m.lower, ymax = m.upper), fill = "red", alpha =0.1) +
        geom_line(data = filter(data2,is.na(estimate)) , aes(y=estimate.merged.smooth), color = "blue", lty=2) +
        geom_ribbon(data = data2, aes(ymin = lower.merged.pred, ymax = upper.merged.pred), fill = "blue", alpha = 0.2) +
        geom_line(data = filter(data2,!is.na(estimate)) , aes(y=estimate.merged.smooth), color = "blue") +
        xlab("Dia dos sintomas") +
        ylab("N de casos diários SRAG sem filtro") +
        ggtitle(sigla) +
        theme_bw()
    p1
}

## Funcao para graficos semanais
plot.seman <- function(sigla){
    max.sem <- observ.data.max$sem.max[observ.data.max$estado==sigla]
    data1  <-  infogr.estado %>% filter(sigla.estado == sigla) 
    data2 <- observ.seman %>% filter(semana <= (max.sem) & sigla.estado == sigla)
    p1 <- ggplot(data1, aes(semana)) +
        geom_line(aes(y=casos.obs), lty =2 , col = "red") +
        geom_line(aes(y=casos.est), col = "red") +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha =0.1) +
        geom_line(data = data2, aes(y=casos.obs), lty =2 , col = "blue") +
        geom_line(data = data2, aes(y=casos.est), col = "blue") +
        geom_ribbon(data = data2, aes(ymin = lower, ymax = upper), fill = "blue", alpha =0.1) +
        xlab("Semana epidemiológica dos sintomas") +
        ylab("N de casos semanais SRAG sem filtro") +
        ggtitle(sigla) +
        theme_bw()
    p1
}

## Plota os dois graficos lado a lado
plota2 <- function(sigla){
    grid.arrange(
        plot.seman(sigla),
        plot.diario(sigla),
        nrow = 1, ncol =2)
}

################################################################################
## Gera o relatorio
################################################################################
sink(file = "relat_body.Rmd")
for(regiao in unique(dic.estados$regiao)){
    cat("# Região", regiao, "\n \n")
    for(sigla in sort(dic.estados$sigla[dic.estados$regiao == regiao])){
        cat("##", dic.estados$nome[dic.estados$sigla == sigla], "\n \n")
        cat("```{r } \n plota2(\"",sigla,"\") \n``` \n \n", sep="")
    }
}
sink()

render("relatorio_InfogripeXObservatorio.Rmd", output_dir="output")
