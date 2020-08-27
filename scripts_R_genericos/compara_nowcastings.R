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
library(purrr)
library(devtools)
## source("../nowcasting/fct/get.last.date.R")
## source("../nowcasting/fct/get_dates.R")
## source("../nowcasting/fct/get.data.base.R")
## source("../nowcasting/fct/check.geocode.R")
load_all("../../now_fcts/")

################################################################################
## leitura das últimas planilhas de nowcasting de casos do Observatório
################################################################################
data.dir <- "../site/dados/municipios/SP/Sao_Paulo/tabelas_nowcasting_para_grafico/"
datas <- get.data.base(data.dir, last=FALSE)
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

################################################################################
## Comparacao de nowcasting de 40 dias atrás com o observado 40 dias após
################################################################################
source("comparacao_bases/format_all_nowcasting.R")
source("comparacao_bases/check_diario.R")

diario <- format_nowcasting_output(
  dir = "../site/dados/",
  escala = "municipio",
  geocode = "3550308",
  analise = c("nowcasting_diario"))

datas_now <- get.data.base(names = list.files(data.dir), last = FALSE)
## Que data está a 40 dias da última
data_fim <- last(datas_now[as.Date(datas_now,"%Y_%m_%d") <= as.Date(datas_now[length(datas_now)],"%Y_%m_%d")-40])
compara_nowcastings_diarios(df = diario, 
                            tipo = "srag",
                            data_inicial = data_fim,
                            data_final = datas_now[length(datas_now)],
                            plot.points = FALSE) + ggtitle("COVID")
