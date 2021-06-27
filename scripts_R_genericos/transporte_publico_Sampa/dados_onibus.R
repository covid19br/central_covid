library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(aweek)
library(stringr)
library(RCurl)
source("../../nowcasting/fct/get.last.date.R")

################################################################################
## Importacao da planilhas de n de passageiros da Prefeitura
## De https://www.prefeitura.sp.gov.br/cidade/secretarias/transportes/institucional/sptrans/acesso_a_informacao/agenda/index.php?p=292723
################################################################################
## Ja executadas, nao precisa repetir
## Janeiro de 2019 ##
## de 01 a 08
## jan2019 <- format(as.Date("2019-01-01") + 0:7, "%d%m%Y")
## datas.lab <- toupper(format(as.Date("2019-01-01") + 0:7, "%d%b%Y"))
## for(i in 1:length(jan2019)){
##     uerrele <- paste0("https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/",jan2019[i],".xls")
##     if(url.exists(uerrele))
##         download.file(uerrele, destfile = paste0("onibus/",datas.lab[i],".xls"))
## }
## ## De 08 a 26 de jan 2019
## jan2019 <- format(as.Date("2019-01-09") + 0:17, "%d%m%Y")
## datas.lab <- toupper(format(as.Date("2019-01-09") + 0:17, "%d%b%Y"))
## for(i in 1:length(jan2019)){
##     uerrele <- paste0("https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/",jan2019[i],
##                       "_Dados%20para%20Transpar%C3%AAncia.xls")
##     if(url.exists(uerrele))
##         download.file(uerrele, destfile = paste0("onibus/",datas.lab[i],".xls"))
## }
## ## entre 27 jan  e 28 de dev:
## fev2019 <- toupper(format(as.Date("2019-01-27") + 0:32, "%d-%b"))
## datas.lab <- toupper(format(as.Date("2019-01-27") + 0:32, "%d%b%Y"))
## for(i in 1:length(fev2019)){
##     uerrele <- paste0("https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/",fev2019[i],".xls")
##     if(url.exists(uerrele))
##         download.file(uerrele, destfile = paste0("onibus/",datas.lab[i],".xls"))
## }

## A partir de março de 2019 ##
## Datas para tentar: todas até o dia de hoje
## ATENCAO as planilha no site de 1 a 4 de jen de 2019 tem dados de 2018, aparentemente
## Por isso comecei com 05-01-2019
datas <- as.Date("2019-01-05") + 0:(Sys.Date() - as.Date("2019-01-01"))
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
onibus.19.21 <- data.frame(data = dados$Data[1], tot.pass = sum( dados[,"Tot Passageiros Transportados"]))
## O loop: vai colando linha a este data.frame
dir.all <- str_sub(dir("onibus/"), end = 9)
nomes <- dir.all[ dir.all %in% datas.texto]
for(i in  nomes){
    dados <-  read_xls(paste0("onibus/",i,".xls"), skip=1)
    dados2 <- data.frame(data = dados$Data[1], tot.pass = sum( dados[,"Tot Passageiros Transportados"]))
    onibus.19.21 <- rbind(onibus.19.21, dados2)
}

onibus.19.21$data <- as.Date(onibus.19.21$data)
## Correcoes de erros. Na mao por enquanto, para documentar
## Tem um dado discrepante em abril, removendo
onibus.19.21[onibus.19.21$data>as.Date("2020-04-01")&onibus.19.21$tot.pass>6e6,] <- NA
## Duas datas com ano errado (12/5 e 13/5)
onibus.19.21$data[onibus.19.21$data==as.Date("2019-05-12")]  <-  as.Date("2020-05-12")
onibus.19.21$data[onibus.19.21$data==as.Date("2019-05-13")]  <-  as.Date("2020-05-13")
## Por semana
set_week_start(7) ## define inicio da semana como domingo
onibus.19.21 %<>%
    arrange(data) %>%
    mutate(semana = date2week(data, floor_day = TRUE),
           semana.num = date2week(data, numeric = TRUE),
           semana.dia1 = week2date(semana),
           ano = format(data, "%Y"))
## Por semana
onibus.sem  <- 
    onibus.19.21 %>%
    group_by(semana.dia1) %>%
    summarise(mean.pass=mean(tot.pass, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(dif.53 = 100*(mean.pass- lag(mean.pass, 53))/mean.pass)

## Graficos
png("passageiros_onibus_19_21_sampa%1d.png", width = 600)
onibus.19.21 %>%
    fortify() %>%
    ggplot(aes(data, tot.pass)) +
    geom_line(col="grey") +
    ##geom_point() +
    theme_bw() +
    xlab("Data") +
    ylab("N de passageiros")

onibus.sem %>%
    ##fortify() %>%
    ggplot(aes(semana.dia1, mean.pass)) +
    geom_line(col="grey") +
    geom_point() +
    theme_bw() +
    xlab("Data início da semana epidemiológica") +
    ylab("Média passageiros/dia")

onibus.sem %>%
    fortify() %>%
    filter(!is.na(dif.53)) %>%
    ggplot(aes(semana.dia1, dif.53)) +
    geom_line(col="grey") +
    geom_point() +
    theme_bw() +
    xlab("Data início da semana epidemiológica") +
    ylab("Dif % relativa a 53 semanas antes")

dev.off()

write.csv2(onibus.19.21, file = "passageiros_onibus_sptrans.csv", row.names = FALSE)
