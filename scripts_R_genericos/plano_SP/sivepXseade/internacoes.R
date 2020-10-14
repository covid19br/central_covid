library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(zoo)
library(aweek)
library(stringr)
library(RCurl)
library(caTools)
library(gridExtra)
source("../../nowcasting/fct/get.last.date.R")
source("../../nowcasting/fct/check.geocode.R")

## Internacoes repo SEADE, do dia 27 de agosto de 2020
seade <- read.csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/d2c21f285dffa9913ad5bfad49b6ad4de8ebc956/data/plano_sp_leitos_internacoes.csv")
## Junta dados da grande sp
seade.drs1 <- aggregate(seade[grepl("Grande", seade$nome_drs),3:8],
                        by = list(datahora=seade$datahora[grepl("Grande", seade$nome_drs)]), sum)
seade.drs1$internacoes_7v7 <- with(seade.drs1, 100*(internacoes_7d - internacoes_7d_l)/internacoes_7d_l)
seade.drs1$nome_drs <- "DRS 01 Grande São Paulo"
## Junta com o dado original
seade <- rbind(seade, seade.drs1[,names(seade)])
seade$datahora <- as.Date(seade$datahora)

## Tabela de correspondencia entre nomes na planilha seade e na planilha Observatorio
nomes.DRS <- data.frame(nomes.sead= sort(unique(seade$nome_drs[grepl("DRS", seade$nome_drs)])),
                        nomes.obs=dir("../../site/dados/DRS/SP/")[c(8, 1:7, 9:17)])


## Funcao para calcular o indice de variacao: n de casos nos ultimos 7 dias - n casos 7 dias antes / n casos 7 das antes
indice.SP <- function(x){
    x_7d <- runmean(x, k = 7, align = "right", endrule="NA")*7
    x_7d_l <- lag(x_7d, 7)
    100*(x_7d - x_7d_l)/x_7d_l
}
## Funcao para ler dados do Observatorio, claular indice do plano e juntar com dados SEADE
tacalepau <- function(local,
                      data = get.last.date(paste0("../../dados_processados/nowcasting/DRS/SP/", local,"/output_nowcasting/"))){
    data.dir <- paste0("../../site/dados/DRS/SP/", local,"/tabelas_nowcasting_para_grafico/")
    ##data <- get.last.date(data.dir)
    srag.now <- read.csv(paste0(data.dir,"nowcasting_diario_srag_", data,".csv"))
    srag.now$data <- as.Date(srag.now$data)
    ## Casos SRAG por data de notificação sem nowcasting
    data.dir2 <- paste0("../../dados_processados/nowcasting/DRS/SP/", local,"/output_nowcasting/")
    srag.not <- read.csv(paste0(data.dir2,"notificacoes_srag_", data,".csv"))
    srag.not$dt_notific <- as.Date(srag.not$dt_notific)
    obj <- merge(
        srag.now[,c("data","estimate.merged")], srag.not, by.x = "data", by.y = "dt_notific", all=TRUE)
    names(obj) <- c("data","nowcasting", "notificados")
    obj$now_7d  <- runmean(obj$nowcasting, k = 7, align = "right", endrule="NA")*7
    obj$not_7d  <- runmean(obj$notificados, k = 7, align = "right", endrule="NA")*7
    obj$now_7v7 <- indice.SP(obj$nowcasting)
    obj$not_7v7 <- indice.SP(obj$notificados)
    index <- seade$nome_drs == nomes.DRS$nomes.sead[nomes.DRS$nomes.obs==local]
    result <- merge(seade[index,], obj, by.x = "datahora", by.y="data", all=TRUE)
    result$nome.obs <- local
    names(result)[1] <- "data"
    result$data <- as.Date(result$data)
    result
}
## dataframe com todas as DRSs
DRS <- ldply(nomes.DRS$nomes.obs, .fun=tacalepau)
write.csv(DRS, "indice_internacoes_X SIVEP_observatorio.csv", row.names=FALSE)

## Plot acumulados ultimos 7 dias
p1 <- function(x, drs.name, nowcasting = TRUE, legend=FALSE, title=TRUE){
    f1 <- filter(x, nome_drs == drs.name) %>%
        ggplot(aes(data)) +
        geom_line(aes(y=internacoes_7d, color = "SEADE")) +
        geom_line(aes(y=not_7d, color = "Sivep, Notificaçoes")) +
        xlab("") +
        ylab("Novas internações susp. + conf. 7 dias") +
        theme_bw() +
        theme(legend.position = c(0.8,0.9), legend.title=element_blank()) +
        ggtitle(drs.name)
    if(nowcasting)
        f1 <- f1 + geom_line(aes(y=now_7d, color = "Sivep, Nowcasting"))
    if(legend)
        f1 <- f1+ theme(legend.position = c(0.8,0.9), legend.title=element_blank())
    if(!legend)
        f1 <- f1 + theme(legend.position = "none")
    if(title)
        f1 <- f1+ ggtitle(drs.name)
    f1
}

## Plot de indices
p2 <-  function(x, drs.name, nowcasting = TRUE, legend=FALSE, title=TRUE){
    f1 <- filter(x, nome_drs == drs.name) %>%
        ggplot(aes(data)) +
        geom_line(aes(y=internacoes_7v7, color = "SEADE")) +
        geom_line(aes(y=not_7v7, color = "Sivep, Notificaçoes")) +
        xlab("") +
        ylab("% Variação novas internações 7 dias") +
        geom_hline(yintercept = 0, lty=2)+
        theme_bw()
    if(nowcasting)
        f1 <- f1 + geom_line(aes(y=now_7v7, color = "Sivep, Nowcasting")) 
    if(legend)
        f1 <- f1+ theme(legend.position = c(0.8,0.9), legend.title=element_blank())
    if(!legend)
        f1 <- f1 + theme(legend.position = "none")
    if(title)
        f1 <- f1+ ggtitle(drs.name)
    f1
}
## Funcao para combinar os dois
p3 <- function(drs.name, x, ...){
    grid.arrange( p1(x, drs.name, ...), p2(x , drs.name, title = FALSE, ...),
                 ncol=1, nrow=2)
    }

## Os graficos
pdf("PlanoSPxSivep.pdf")
p3("DRS 01 Grande São Paulo", DRS, legend = TRUE)
lapply(unique(nomes.DRS$nomes.sead)[-1], p3, DRS)
dev.off()

pdf("PlanoSPxSivep_sem_nowcasting.pdf")
p3("DRS 01 Grande São Paulo", DRS, nowcasting=FALSE, legend=TRUE)
lapply(unique(nomes.DRS$nomes.sead)[-1], p3, DRS, nowcasting=FALSE)
dev.off()


