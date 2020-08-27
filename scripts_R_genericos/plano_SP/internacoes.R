library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(aweek)
library(stringr)
library(RCurl)
library(caTools)
source("../../nowcasting/fct/get.last.date.R")
source("../../nowcasting/fct/check.geocode.R")

## Internacoes repo SEADE
seade <- read.csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes.csv")
## Junta dados da grande sp
seade.drs1 <- aggregate(seade[grepl("Grande", seade$nome_drs),3:8],
                        by = list(datahora=seade$datahora[grepl("Grande", seade$nome_drs)]), sum)
seade.drs1$internacoes_7v7 <- with(seade.drs1, (internacoes_7d - internacoes_7d_l)/internacoes_7d_l)
seade.drs1$nome_drs <- "Grande São Paulo"
## Junta com o dado original
seade <- rbind(seade, seade.drs1[,names(seade)])
seade$datahora <- as.Date(seade$datahora)


## sufixo do diretorio
local <- check.geocode(escala="estado", geocode = 35, sigla ="SP")
## Casos SRAG com nowcasting
data.dir <- pa ste0("../../site/dados/", local,"/tabelas_nowcasting_para_grafico/")
data <- get.last.date(data.dir)
srag.now <- read.csv(paste0(data.dir,"nowcasting_diario_srag_", data,".csv"))
## Casos SRAG por data de notificação sem nowcasting: não está dando certo pq o diretório dados_processados/nowcasting nao esta com cvs atualizados
data.dir2 <- paste0("../../dados_processados/nowcasting/", local,"/output_nowcasting/")
srag.now <- read.csv(paste0(data.dir2,"notificacoes_srag_", data,".csv"))


