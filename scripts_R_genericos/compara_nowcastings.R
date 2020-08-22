library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(zoo)
library(aweek)
library(stringr)
library(RCurl)
library(car)
library(bbmle)
source("../nowcasting/fct/get.last.date.R")
source("../nowcasting/fct/get_dates.R")

################################################################################
## leitura das últimas  planilhas de nowcasting de casos do Observatório
################################################################################
data.dir <- "../site/dados/municipios/SP/Sao_Paulo/tabelas_nowcasting_para_grafico/"
datas <- get_dates(data.dir)
f1 <- function(data, data.dir){
    data.lab <- format(data, "%Y_%m_%d")
    dados <-  read.csv(paste0(data.dir,"nowcasting_diario_srag_", data.lab,".csv"))
    dados$dt_base  <- data
    dados$data <- as.Date(dados$data)
    dados
}

## Comparando os últimos nowcastings
n <- 4
ldply( lapply(rev(datas)[1:n], f1, data.dir = data.dir)) %>%
    filter(data >= min(rev(datas)[1:n])-40) %>%
    ggplot(aes(x=data)) +
    geom_line(aes(y = estimate.merged, color=factor(dt_base))) +
    geom_ribbon(aes(ymin=lower.merged.pred, ymax = upper.merged.pred, fill = factor(dt_base)), alpha =0.1) +
    theme_bw()

