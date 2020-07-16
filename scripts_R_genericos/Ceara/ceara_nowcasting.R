library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(NobBS)
library(zoo)
source("../../nowcasting/fct/beta.cumsum.R")
source("../../nowcasting/fct/beta.summary.R")
source("../../nowcasting/fct/median_delay.R")
## Leitura dos dados, baixados de https://indicadores.integrasus.saude.ce.gov.br/indicadores/indicadores-coronavirus/coronavirus-ceara

dados <- read.csv("ceara_boletim_2020_07_15.csv", row.names=1, na.strings = c("NA", " ", ""))
##dt.index <- grepl("data", names(dados))
##dados[,dt.index] <- sapply(dados[,dt.index], as.Date, "%Y-%m-%d %H:%M:%S")
positivos  <- dados %>%
    filter(resultadoFinalExame == "Positivo") %>%
    mutate(dt_sin_pri = as.Date(dataInicioSintomas,"%Y-%m-%d %H:%M:%S"),
           dt_not = as.Date(dataNotificacao,"%Y-%m-%d %H:%M:%S"),
           dt_resul = as.Date(dataResultadoExame,"%Y-%m-%d %H:%M:%S"),
           dt_record = pmax(dt_not,dt_resul, na.rm=TRUE)) %>%
    filter(!is.na(dt_sin_pri) & !is.na(dt_record)) %>% #rever para fazer imputação
    select(dt_sin_pri, dt_record)


## N de casos
n.casos <- positivos %>%
    group_by(dt_sin_pri) %>%
    summarise(n.casos = n()) %>%
    as.data.frame()
n.casos.zoo <- zoo(n.casos[,-1], n.casos[,1])

## Nowcasting
now.ce <- NobBS(positivos,
                now = max(positivos$dt_sin_pri) - 5,
                units = "1 day",
                onset_date = "dt_sin_pri",
                report_date = "dt_record",
                moving_window = 40)
now.ce.zoo <- zoo(now.ce$estimates[, -c(4:5)],now.ce$estimates$onset_date)
tudo <- merge(n.casos = n.casos.zoo, now.ce.zoo)

## Delay mediano de notificação
median_delay(NobBS.output = now.ce, samples = 100)


## PLots
png("ceara_nowcasting_2020_07+14.png", width = 600)
ggplot(tudo, aes(Index)) +
    geom_line(aes(y=n.casos, color = "Confirmados")) +
    geom_line(aes(y = estimate, color = "Nowcasting")) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill="blue", alpha = 0.2)
dev.off()
