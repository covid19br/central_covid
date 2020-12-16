library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(aweek)
library(stringr)
library(RCurl)
library(car)
library(bbmle)
source("../../nowcasting/fct/get.last.date.R")

################################################################################
## Importacao da planilhas de n de passageiros da Prefeitura
## De https://www.prefeitura.sp.gov.br/cidade/secretarias/transportes/institucional/sptrans/acesso_a_informacao/agenda/index.php?p=292723
################################################################################
## Datas para tentar: todas até o dia de hoje
datas <- as.Date("2020-01-01") + 0:(Sys.Date() - as.Date("2020-01-01"))
datas.texto <- toupper((format(datas, "%d%b%Y")))
## Verifica arquivos já baixados
datas.velhas <- str_sub(dir("onibus/"), end = 9) ## pega só a data dos arquivos, que são uma parte do nome dos arquivos
## seleciona as datas novas
datas.novas  <- datas.texto[!(datas.texto %in% datas.velhas)]
## Verifica se há arquivos para estas datas e baixa os que existem
for(i in 1:length(datas.novas)){
    uerrele <- paste0("https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/",datas.novas[i],".xls")
    if(url.exists(uerrele))
        download.file(uerrele, destfile = paste0("onibus/",datas.novas[i],".xls"))
}

## Junta todos em um unico arquivo com total de passageiros por dia
## PIP: sim, é um loop com um rbind dentro. Feio mas funciona, aguardo experts para dar mais elegância e efetividade a esta parte
## Primeiro passo do loop: cria um data.frame com uma linha
dados <-  read_xls(paste0("onibus/",datas.texto[1],".xls"), skip=1)
onibus.2020 <- data.frame(data = dados$Data[1], tot.pass = sum( dados[,"Tot Passageiros Transportados"]))
## O loop: vai colando linha a este data.frame
for(i in 2:length(dir("onibus/")) ){
    dados <-  read_xls(paste0("onibus/",datas.texto[i],".xls"), skip=1)
    dados2 <- data.frame(data = dados$Data[1], tot.pass = sum( dados[,"Tot Passageiros Transportados"]))
    onibus.2020 <- rbind(onibus.2020, dados2)
}

onibus.2020$data <- as.Date(onibus.2020$data)
## Correcoes de erros. Na mao por enquanto, para documentar
## Tem um dado discrepante em abril, removendo
onibus.2020[onibus.2020$data>as.Date("2020-04-01")&onibus.2020$tot.pass>6e6,] <- NA
## Duas datas com ano errado (12/5 e 13/5)
onibus.2020$data[onibus.2020$data==as.Date("2019-05-12")]  <-  as.Date("2020-05-12")
onibus.2020$data[onibus.2020$data==as.Date("2019-05-13")]  <-  as.Date("2020-05-13")
## Por semana
set_week_start("Sunday")
onibus.2020$semana <- date2week(onibus.2020$data, numeric=TRUE)
onibus.sem  <- 
    onibus.2020 %>%
    group_by(semana) %>%
    summarise(mean.pass=mean(tot.pass, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(mean.pass.rel = mean.pass/max(mean.pass, na.rm=TRUE))
png("passageiros_onibus_semana_sampa%1d.png")
plot(mean.pass ~ semana, data= onibus.sem,
     type = "b",
     xlab = "semana epidemiológica", ylab = "Média passageiros/dia",
     subset=semana<51
     )
## Relativo
plot(mean.pass.rel ~ semana, data= onibus.sem,
     type = "b",
     xlab = "semana epidemiológica", ylab = "Média passageiros/dia, relativo maximo",
     subset=semana<51)
abline(h=0.5, lty=2, col="red")
abline(h=0.6, lty=2, col="blue")
dev.off()


################################################################################
## Leitura da tabela ridicula e zoada de indice de isolamento do Estado
## https://www.saopaulo.sp.gov.br/coronavirus/isolamento/
################################################################################
## isolam <- read.csv2("simi_sampa_2020_07_24.csv", na.strings="")
## isolam$data <- as.Date(paste0(str_sub(isolam$Data, -5, -1), "/2020"), "%d/%m/%Y")
## isolam$indice <- as.integer(str_sub(isolam[,4], 1,2))
## isolam.zoo  <- zoo(isolam$indice, isolam$data)

## Leitura do indice do mesmo indice de isolamento, fornecido pelo Pedro Peixoto (dados restrito, nao esta no repo)
isolam <- read.csv("../../balaio/dados/Social Distancing Index by Cities2020_08_13.csv")%>%
    filter(state_name=="São Paulo" & city_name=="São Paulo")
isolam.zoo <- zoo(isolam$isolated, as.Date(isolam$dt))
isolam$semana <- date2week(isolam$dt, numeric=TRUE)
isolam.sem  <- 
    isolam %>%
    group_by(semana) %>%
    summarise(mean.isol=mean(isolated, na.rm=TRUE))
png("isolamento_semana_sampa.png")
plot(mean.isol ~ semana, data= isolam.sem,
     type = "b",
     xlab = "semana epidemiológica", ylab = "Isolamento médio",
     subset=semana<33)
dev.off()

################################################################################
## leitura da última planilha de R efetivo  e nowcasting de casos do Observatório
################################################################################
data.dir <- "../../site/dados/municipios/SP/Sao_Paulo/tabelas_nowcasting_para_grafico/"
data <- get.last.date(data.dir)
reff <- read.csv(paste0(data.dir,"r_efetivo_covid_", data,".csv"))
reff.zoo <- zoo(reff$Mean.R, as.Date(reff$data))
casos <- read.csv(paste0(data.dir,"nowcasting_diario_covid_", data,".csv"))
casos.cum <- read.csv(paste0(data.dir,"nowcasting_acumulado_covid_", data,".csv")) %>%
    filter(as.Date(data) <= max(as.Date(casos$data)))

casos.zoo <-  zoo(
    data.frame(casos = casos$estimate.merged,
               casos.cum = casos.cum$now.mean.c,
               d.casos = c(diff(casos$estimate.merged), NA),
               d.casos.smooth = c(diff(casos$estimate.merged.smooth), NA)),
    as.Date(casos$data))
################################################################################
## Junta todas as series temporais em um unico objeto zoo
################################################################################
onibus.2020.zoo <- zoo(onibus.2020[,-1], onibus.2020[,1]) #%>% window(end = max(as.Date(casos$data)))
tudo <- merge(casos.zoo, passageiros=onibus.2020.zoo)
tudo <- merge.zoo(tudo, R.eff=reff.zoo)
tudo <- merge.zoo(tudo, isolamento =isolam.zoo)
tudo$passageiros.smooth <- rollapply(tudo$passageiros, width = 7, mean, partial = TRUE)

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
              mean.R = mean(R.eff, na.rm=TRUE),
              mean.casos = mean(casos, na.rm=TRUE),
              sum.casos = sum(casos, na.rm=TRUE),
              casos.cum = max(casos.cum, na.rm=TRUE),
              mean.isolam = mean(isolamento),
              nobs = sum(!is.na(casos))) %>%
    mutate(casos.cum = ifelse(casos.cum==-Inf, NA, casos.cum)) %>%
    filter(nobs ==7) %>%
    as.data.frame()
tudo.sem$d.casos <- c(diff(tudo.sem$sum.casos),NA)
tudo.sem <- zoo(tudo.sem[,-1], tudo.sem[,1])

################################################################################
## Graficos exploratorios
################################################################################
## Series temporais diárias
plot(tudo[, c(5,7, 1, 6)])
## Correlacoes
plot(isolamento ~ passageiros, data = fortify(tudo))
plot(R.eff ~ lag(passageiros.smooth,5), data = window(tudo, start=as.Date("2020-04-01")),
     xlab = "N de passageiros 5 dias antes", ylab = "R efetivo")
plot(R.eff ~ casos.cum, data = window(tudo, start=as.Date("2020-04-01")),
     xlab = "casos srag confirmados acumualdos", ylab = "R efetivo")
plot(casos ~ lag(passageiros,5), data = window(tudo, start=as.Date("2020-04-01")))
plot(d.casos ~ lag(passageiros,5), data = window(tudo, start=as.Date("2020-04-01")))
plot(d.casos ~ casos.cum, data = window(casos.zoo))
plot(d.casos.smooth ~ casos.cum, data = window(casos.zoo))
## avplots
avPlots(lm(d.casos ~ lag(passageiros.smooth,5) + casos.cum, data = tudo))
avPlots(lm(d.casos.smooth ~ lag(passageiros,5) + casos.cum, data = tudo))
avPlots(lm(R.eff ~ lag(passageiros.smooth,5) + casos.cum, data = tudo))
avPlots(lm(R.eff ~ lag(passageiros.smooth,5) * casos.cum, data = tudo))

## Modelos de Regressao
tudo.df <- as.data.frame(tudo)
tudo.df$pass.l5 <- lag(tudo.df$passageiros.smooth,5)
tudo.df <- filter(tudo.df, !is.na(R.eff) & !is.na(pass.l5) & !is.na(casos.cum))
m0 <- lm(R.eff ~ 1, data = tudo.df)
m1 <- lm(R.eff ~ casos.cum, data = tudo.df)
m2 <- lm(R.eff ~ pass.l5, data = tudo.df)
m3 <- lm(R.eff ~ casos.cum + pass.l5, data = tudo.df)
m4 <- lm(R.eff ~ casos.cum * pass.l5, data = tudo.df)
AICctab(m1,m2,m3,m4)
summary(m4)
## Previstos
tudo.df$data <- as.Date(rownames(tudo.df))
plot( R.eff ~ data, type ="l", col=2, data=tudo.df)
lines(fitted(m4) ~ tudo.df$data, type= "l")
lines(fitted(m3) ~ tudo.df$data, type= "l", col=3)
## Previstos
newdata <- data.frame(casos.cum= max(tudo.df$casos.cum), pass.l5 = seq(3e6, 7.5e6, by=5e5))
predict(m4, newdata = newdata, interval ="prediction")
## Graficos por semana epidemiologica
## Serie temporais
plot(tudo.sem)
## Correlacoes
plot(mean.R ~ lag(mean.pass,1), data = tudo.sem)
plot(mean.R ~ casos.cum, data = tudo.sem)
plot(mean.casos ~ lag(mean.pass,1), data = tudo.sem)
plot(mean.casos ~ lag(mean.pass,1), data = tudo.sem, subset = semana>12)
plot(d.casos ~ casos.cum, data = tudo.sem)
avPlots(lm(d.casos ~ lag(mean.pass,1) + casos.cum, data = tudo.sem))
avPlots(lm(mean.R ~ lag(mean.pass,1) + casos.cum, data = tudo.sem))


## modelos de regressao
tudo.sem.df <- as.data.frame(tudo.sem)
tudo.sem.df$mean.pass.l1 <- lag(tudo.sem.df$mean.pass,1)
tudo.sem.df <- filter(tudo.sem.df, !is.na(mean.pass.l1))
tudo.sem.df$semana  <- as.integer(rownames(tudo.sem.df))
m0 <- lm(mean.R ~ 1, data = tudo.sem.df)
m1 <- lm(mean.R ~ casos.cum, data = tudo.sem.df)
m2 <- lm(mean.R ~ mean.pass.l1, data = tudo.sem.df)
m3 <- lm(mean.R ~ casos.cum + mean.pass.l1, data = tudo.sem.df)
m4 <- lm(mean.R ~ casos.cum * mean.pass.l1, data = tudo.sem.df)
AICctab(m0, m1, m2, m3, m4)

plot(mean.R ~semana, data=tudo.sem.df, type ="l")
lines(fitted(m3) ~ tudo.sem.df$semana, col= 2)

## Previsoes
newdata  <-  data.frame(casos.cum = max(tudo.sem.df$casos.cum), mean.pass.l1= seq(3.5e6, 5e6, by = 5e5))
predict(m3, newdata =newdata, interval = "prediction")
par(mfrow=c(2,2))
plot(m3)
