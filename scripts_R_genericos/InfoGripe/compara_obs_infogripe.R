library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(zip)
library(aweek)
source("../../nowcasting/fct/get.last.date.R")
set_week_start("Sunday")

## Dicionario de nomes de estados
dic.territ <- read.csv("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/base_territorial/DICIONARIO_territorios_id.csv")

################################################################################
## Planilha com nowcasting dos estados do site do Observatorio
################################################################################
dir1 <- "../../site/dados/estado/"
siglas.estado <- dir(dir1)
dir2 <- paste0(dir1,siglas.estado[1],"/tabelas_nowcasting_para_grafico/")
data <- get.last.date(dir2)
tmp <- read.csv(paste0(dir2,"nowcasting_diario_srag_",data,".csv"))
tmp$estado <- siglas.estado[1]
srag.obs <- tmp[,c(8,1:7)]
for(i in 2:length(siglas.estado)){
    dir2 <- paste0(dir1,siglas.estado[i],"/tabelas_nowcasting_para_grafico/")
    data <- get.last.date(dir2)
    tmp <- read.csv(paste0(dir2,"nowcasting_diario_srag_",data,".csv"))
    tmp$estado <- siglas.estado[i]
    srag.obs <- rbind(srag.obs, tmp[,c(8,1:7)])
}
srag.obs$data <- as.Date(srag.obs$data)
## ## Agrega por semana
## srag.sem <-
##     srag.obs %>%
##     mutate(semana = date2week(data, numeric = TRUE)) %>%
##     group_by(estado, semana) %>%
##     summarise(casos.obs = sum(estimate.merged),
##               casos.est = sum(estimate.merged),
##               lower = sum(lower.merged.pred),
##               upper = sum(upper.merged.pred)) %>%
##     ungroup() %>%
##     mutate(casos.obs = ifelse(is.na(lower), casos.obs, NA), casos.est = ifelse(is.na(lower), NA, casos.est)) %>%
##     merge(dic.territ[, 1:3], by.x = "estado", by.y = "sigla") %>%
##     mutate(sigla.estado = estado, id.estado = id, nome.estado = nome, base = "Observatório") %>%
##     select(id.estado, sigla.estado, nome.estado, semana, casos.obs, casos.est, lower, upper, base) %>%  
##     as.data.frame()

################################################################################
## Dados do Infogripe
################################################################################
infogripe <- read.csv2("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_sintomas.csv")
infogripe$data.de.publicação <- as.Date(infogripe$data.de.publicação)
info.estado <- filter(infogripe, Tipo == "Estado"& Ano.epidemiológico > 2019 &
                                 data.de.publicação == max(data.de.publicação) & dado == "srag" &
                                 escala == "casos")[, c(2,3,8,10,11,12,14)]
info.estado <- merge(info.estado, dic.territ[, 1:2], by.x = "UF", by.y = "id")
names(info.estado)[1:8] <- c("id.estado", "nome.estado", "semana", "casos.obs", "lower", "casos.est", "upper", "sigla.estado")
info.estado$base <- "InfoGripe"
##info.estado <- info.estado[, names(srag.sem)]
## Trnasforma semana epidemiol em data do ultimo dia dela e divide n de casos por 7
## para plotar na escala diária
info.estado %<>%
    mutate(sem=ifelse(nchar(semana)==1, paste0("0",semana), as.character(semana)),
                       data = week2date(as.aweek(paste0("2020-W",sem,"-7"))),
           m.casos.obs = casos.obs/7, m.lower = lower/7, m.casos.est = casos.est/7, m.upper = upper/7)


## Funcao rapidinha para Graficos

plot1 <- function(sigla){
    data1  <- fortify(info.estado) %>% filter(sigla.estado == sigla) 
    data2 <- fortify(srag.obs) %>% filter(estado == sigla) # & !is.na(estimate))
    
    p1 <- ggplot(data1, aes(x=data)) +
        geom_line(aes(y=m.casos.obs), lty = 2, col ="red") +
        geom_line(aes(y=m.casos.est), col = "red") +
        geom_ribbon(aes(ymin = m.lower, ymax = m.upper), fill = "red", alpha =0.1) +
        geom_line(data = filter(data2,is.na(estimate)) , aes(y=estimate.merged.smooth), color = "blue", lty=2) +
        geom_ribbon(data = data2, aes(ymin = lower.merged.pred, ymax = upper.merged.pred), fill = "blue", alpha = 0.2) +
        geom_line(data = filter(data2,!is.na(estimate)) , aes(y=estimate.merged.smooth), color = "blue") +
        xlab("") +
        ylab("N de casos diários SRAG sem filtro") +
        ggtitle(sigla) +
        theme_bw()
    print(p1)
}

pdf("InfogripeXObservatorio_semana_40.pdf", onefile = TRUE)
for(n in siglas.estado)
    plot1(n)
dev.off()
