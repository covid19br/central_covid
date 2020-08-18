library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(readr)
library(lubridate)
library(NobBS)
source("../nowcasting/fct/gera.nowcasting.R")
source("../nowcasting/fct/write.notificacoes.data.R")
source("../nowcasting/fct/prepara_dados2.R")
source("../nowcasting/fct/get.last.date.R")
source("../nowcasting/fct/read.sivep.R")
source("../nowcasting/fct/beta.cumsum.R")
source("../nowcasting/fct/beta.summary.R")
source("../nowcasting/fct/median_delay.R") 

## Leitura dos dados: sivep residentes São Paulo
## escreva aqui a data desejada
## ultima versao
data.base <- get.last.date(data.dir)
## 20 de maio
data.base <- "2020_05_20"
## Leitura
data.dir <- "../dados/estado_SP/SRAG_hospitalizados/dados/"
dados <- read.sivep(dir = data.dir, escala = "municipio",
                    geocode = 3550308, data = data.base) 
## Usando a gera_nowcasting
covid.ob.now <-  gera.nowcasting(dados, caso = FALSE, tipo = "covid",
                               hospitalizados = FALSE, trim.now = 0, window = 40) 
## Resumo dos betas
beta.summary(NobBS.output = covid.ob.now$now)
beta.cumsum(NobBS.output = covid.ob.now$now, samples = 100)
quantile_delay(NobBS.output = covid.ob.now$now, samples = 1000)

    
## Fazendo na unha
dados2 <-
    dados %>%
    filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
                filter(evolucao == 2) %>%
                ##filter(!is.na(dt_evoluca)) %>%
                mutate(dt_record = pmax(dt_encerra, dt_digita, dt_evoluca,
                                        na.rm = TRUE))
## Quantos obitos sem data de evolucao e/ou data de encerramento
table(is.na(dados2$dt_evoluca), is.na(dados2$dt_encerra))
## Resumo da diferenca entre data de obito e data de encarramento
summary(as.integer(dados2$dt_encerra - dados2$dt_evoluca))

## Nowcasting
dados3 <- filter(dados2, !is.na(dt_evoluca))
## Resumo das diferenças entre data de obito e de registro
summary(as.integer(dados3$dt_record - dados3$dt_evoluca))
## Roda nowcasting
covid.ob.now.2 <- NobBS(data = dados3, now = max(dados3$dt_evoluca)-5,
                      units = "1 day", onset_date = "dt_evoluca",
                      report_date = "dt_record", moving_window =40)
## Atraso mediano
quantile_delay(NobBS.output = covid.ob.now.2, samples = 1000)
## Resumo dos betas
beta.summary(NobBS.output = covid.ob.now.2)
beta.cumsum(NobBS.output = covid.ob.now.2, samples = 100)


## Graficos
## Grafico de n de obitos e nowcasting
dados4  <- dados3 %>%
    group_by(dt_evoluca) %>%
    summarise(N = n())

## Grafico rapido e sujo
ggplot(dados4, aes(dt_evoluca)) +
    geom_line(aes(y=N)) +
    geom_line(data=covid.ob.now$now$estimates, aes(onset_date, estimate), col="red") +
    geom_ribbon(data=covid.ob.now$now$estimates, aes(onset_date, ymin = lower, ymax = upper), fill="red", alpha =0.2)
