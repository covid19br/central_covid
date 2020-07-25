library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(aweek)
library(stringr)
source("../../nowcasting/fct/get.last.date.R")

################################################################################
## Importacao da planilhas de n de passageiros da Prefeitura
################################################################################
## Tod do: mudar para evrificar apenas as planilhas aidna não salvas
## Datas
datas <- as.Date("2020-01-01") + 0:(Sys.Date() - as.Date("2020-01-01") -5)
datas.texto <- toupper(format(datas, "%d%b%Y"))

## Baixa todos os arquivos : Rever para baixar só o que aidna não está no diretório
for(i in 1:length(datas.texto))
    raw <- download.file(paste0("https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/",datas.texto[i],".xls"),
                         destfile = paste0("onibus/",datas.texto[i],".xls"))

## Junta todos em um unico arquivo com total de passageiros por dia

dados <-  read_xls(paste0("onibus/",datas.texto[1],".xls"), skip=1)
onibus.2020 <- data.frame(data = datas[1], tot.pass = sum( dados[,"Tot Passageiros Transportados"]))

for(i in 2:length(datas.texto)){
    dados <-  read_xls(paste0("onibus/",datas.texto[i],".xls"), skip=1)
    dados2 <- data.frame(data = datas[i], tot.pass = sum( dados[,"Tot Passageiros Transportados"]))
    onibus.2020 <- rbind(onibus.2020, dados2)
}
## Tem um dado discrepante em abril
onibus.2020[onibus.2020$data>as.Date("2020-04-01")&onibus.2020$tot.pass>6e6,] <- NA

################################################################################
## Leitura da tabela ridicula e zoada de indice de isolamento
################################################################################
isolam <- read.csv2("simi_sampa_2020_07_24.csv", na.strings="")
isolam$data <- as.Date(paste0(str_sub(isolam$Data, -5, -1), "/2020"), "%d/%m/%Y")
isolam$indice <- as.integer(str_sub(isolam[,4], 1,2))
isolam.zoo  <- zoo(isolam$indice, isolam$data)

################################################################################
## leitura da última planilha de R efetivo  e nocasting de casos do Observatório
################################################################################
data.dir <- "../../dados_processados/nowcasting/municipios/SP/Sao_Paulo//tabelas_nowcasting_para_grafico/"
data <- get.last.date(data.dir)
reff <- read.csv(paste0(data.dir,"r_efetivo_covid_", data,".csv"))
reff.zoo <- zoo(reff$Mean.R, as.Date(reff$data))
casos <- read.csv(paste0(data.dir,"nowcasting_diario_covid_", data,".csv"))
casos.zoo <- zoo(casos$estimate.merged, as.Date(casos$data))

################################################################################
## Junta todas as series temporais em um unico objeto zoo
################################################################################
onibus.2020.zoo <- zoo(onibus.2020[,-1], onibus.2020[,1])
tudo <- merge.zoo(passageiros=onibus.2020.zoo, isolamento =isolam.zoo)
tudo <- merge.zoo(tudo, casos = casos.zoo)
tudo <- merge.zoo(tudo, R.mean=reff.zoo)

################################################################################
## Calculos  de Media por semana epidemiologica
################################################################################
set_week_start("Sunday")
tudo$semana <- date2week(time(tudo), numeric=TRUE)
tudo.sem <-
    tudo %>%
    fortify() %>%
    group_by(semana) %>%
    summarise(mean.pass=mean(passageiros, na.rm=TRUE),
              mean.R = mean(R.mean, na.rm=TRUE),
              mean.casos = mean(casos, na.rm=TRUE),
              mean.isolam = mean(isolamento)) %>% zoo()


################################################################################
## Graficos exploratorios
################################################################################
## Series temporais diárias
plot(tudo[, -5])
## Correlacoes
plot(isolamento ~ passageiros, data = fortify(tudo))
plot(R.mean ~ lag(passageiros,5), data = window(tudo, start=as.Date("2020-04-01")))
plot(casos ~ lag(passageiros,5), data = window(tudo, start=as.Date("2020-04-01")))

## Graficos por semana epidemiologica
## Serie temporais
plot(tudo.sem)
## Correlacoes
plot(mean.R ~ lag(mean.pass,1), data = tudo.sem)
plot(mean.R ~ lag(mean.pass,1), data = tudo.sem, subset = semana>12)
plot(mean.casos ~ lag(mean.pass,1), data = tudo.sem)
plot(mean.casos ~ lag(mean.pass,1), data = tudo.sem, subset = semana>12)
