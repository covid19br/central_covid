library(plyr)
library(dplyr)
library(ggplot2)
library(zip)
library(aweek)
source("../../nowcasting/fct/get.last.date.R")
set_week_start("Sunday")

################################################################################
## Campinas
################################################################################

dir.dados <- "../../dados/municipio_campinas/casos_totais/"
dados <- read.csv2(paste0(dir.dados,"Juncao_", get.last.date(dir.dados),".csv"))

with(dados, table(TIPO_TESTE, BANCO))


## Positividade por semana, E-SUS e SIVEP

## N total de casos por semana
n.casos <-
    dados %>%
    filter(SEM_PRI!="" & (CLASSI_FIN == "COVID" | PCR_SARS2 == "Detect\xe1vel" | RES_TESTE == "Positivo" )) %>%
    mutate(semana = as.integer(gsub("SE ", "", SEM_PRI))) %>%
    group_by(semana) %>%
    summarise(N.casos = n())
## Variacao % de casos de uma semana a outra
n.casos$dif.casos <- c(NA, diff(n.casos$N.casos)) / lag(n.casos$N.casos)

## Positividade E-SUS
positividade <-
    dados %>%
    filter(SEM_PRI!="" & BANCO == "ESUS VE" & TIPO_TESTE == "RT-PCR") %>%
    mutate(semana = as.integer(gsub("SE ", "", SEM_PRI))) %>%
    group_by(semana) %>%
    summarise(testados = n(), positivos = sum(RES_TESTE == "Positivo"), positividade = positivos/testados) %>%
    merge( n.casos, by = "semana")


plot(positividade ~semana, data= positividade)
plot(dif.casos ~ positividade, data = positividade, subset = semana >10 & semana <41)
## Series temporais
par(mfrow = c(3,1))
plot(N.casos ~ semana, data= positividade, type ="b", ylab = "Total de casos")
plot(positividade ~ semana, data= positividade, type ="b", ylab = "Positividade Rt-PCR")
plot(testados ~ semana , data = positividade, type = "b", ylab = "Testes RT-PCR")
par(mfrow = c(1,1))

## Casos x positividade , com e sem lags
plot(N.casos ~ positividade, data = positividade, subset = semana >11 & semana <41)
plot(N.casos ~ lag(positividade, 5), data = positividade, subset = semana >11 & semana <41)
plot(N.casos ~ lag(positividade, 5), data = positividade)
plot(dif.casos ~ lag(positividade, 5), data = positividade, subset = semana >11 & semana <41)

## Trajetorias no espaço casos x positividade
## Sem lag
positividade %>%
    filter(semana > 11 & semana < 41) %>%
    ggplot(aes(x = positividade, y = N.casos)) +
    geom_point() +
    geom_path(lty =2)
## Com lag
positividade %>%
    filter(semana > 11 & semana < 41) %>%
    ggplot(aes(y = N.casos, x = lag(positividade,5))) +
    geom_point() +
    geom_path(lty =2)


## N de positivos x n testados
plot(positivos ~ testados, data = positividade, type = "n", ylab = "Positivos RT-PCR", xlab = "Testados RT-PCR")
text(labels = as.character(positividade$semana),
     y= positividade$positivos,
     x= positividade$testados,
     adj = c(0.5, 0.5))
abline(0,.25, lty = 2, col = "green", lwd=1.5)
abline(0,.5, lty = 2, col = "orange", lwd=1.5)
abline(0,.75, lty = 2, col = "red", lwd = 1.5)
legend("topleft", c("25%", "50%", "75%"), bty = "n", title = "Positividade", lty=2, col =c("green", "orange", "red"))

################################################################################
## Floripa
################################################################################

floripa  <- read.csv("covid_florianopolis.csv") %>%
    mutate(data_primeiros_sintomas = as.Date(data_primeiros_sintomas),
           semana = date2week(data_primeiros_sintomas, numeric = TRUE) )


positividade.fl <-
    floripa %>%
    filter(data_primeiros_sintomas > "2019-12-01" & data_primeiros_sintomas < as.Date(Sys.Date())) %>%
    group_by(semana) %>%
    summarise(N.casos = sum(classificacao_final=="CONFIRMAÇÃO CLÍNICO EPIDEMIOLÓGICO" |
                            classificacao_final== "CONFIRMAÇÃO LABORATORIAL"),
              testados = sum(tipo_teste == "RT-PCR"),
              positivos = sum(tipo_teste == "RT-PCR" & classificacao_final== "CONFIRMAÇÃO LABORATORIAL"),
              positividade = positivos/testados)


plot(N.casos ~ positividade, data = positividade, subset = semana >11 & semana <41)
plot(N.casos ~ lag(positividade, 5), data = positividade, subset = semana >11 & semana <41)
plot(N.casos ~ lag(positividade, 5), data = positividade)
plot(dif.casos ~ lag(positividade, 5), data = positividade, subset = semana >11 & semana <41)

## Series temporais
png("series_semanais_testes_casos_floripa.png", width = 600)
par(mfrow = c(3,1))
plot(N.casos ~ semana, data= positividade.fl, type ="b", ylab = "N de casos", xlab ="", main = "Casos Totais")
plot(positividade ~ semana, data= positividade.fl, type ="b", ylab = "Positividade", xlab ="", main = "Positvidade PCR")
plot(testados ~ semana , data = positividade.fl, type = "b", ylab = "N de testados", main = "RT-PCR realizados")
par(mfrow = c(1,1))
dev.off()

## N de positivos x n testados
png("testesXpositivos_floripa.png")
plot(positivos ~ testados, data = positividade.fl, type = "n", ylab = "N de Positivos",
     xlab = "N de testados", main = "Testes RT-PCR em Florianópolis")
text(labels = as.character(positividade.fl$semana),
     y= positividade.fl$positivos,
     x= positividade.fl$testados,
     adj = c(0.5, 0.5))
abline(0,.5, lty = 2, col = "orange")
abline(0,.25, lty = 2, col = "green")
abline(0,.75, lty = 2, col = "red")
legend("topleft", c("25%", "50%", "75%"), bty = "n", title = "Positividade", lty=2, col =c("green", "orange", "red"))
dev.off()

## Em escala log
positividade.fl %>%
    ggplot(aes(x = positividade, y = N.casos)) +
    geom_point() +
    xlab("Positividade RT-PCR")
    #geom_path(lty =2)

