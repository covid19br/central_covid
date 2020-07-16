library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(aweek)
library(quantreg)
library(NobBS)
library(zoo)
library(purrr)
library(EpiEstim)
library(gridExtra)
## Funcoes do Observatório para nowcasting, R efetivo, plts
source("https://raw.githubusercontent.com/covid19br/nowcasting/master/fct/NobBS.posterior.R")
source("https://raw.githubusercontent.com/covid19br/nowcasting/master/fct/Re.nowcasting.R")
source("https://raw.githubusercontent.com/covid19br/nowcasting/master/fct/R.cori.R")
source("https://raw.githubusercontent.com/covid19br/nowcasting/master/fct/fill.dates.R")
source("https://raw.githubusercontent.com/covid19br/nowcasting/master/fct/posteriors.R")
source("https://raw.githubusercontent.com/covid19br/nowcasting/master/fct/plot.formatos.R")
source("https://raw.githubusercontent.com/covid19br/nowcasting/master/fct/Re.com.data.R")
source("https://raw.githubusercontent.com/covid19br/nowcasting/master/fct/estimate.R0.R")
source("https://raw.githubusercontent.com/covid19br/nowcasting/master/fct/na.zero.R")

################################################################################
## Leitura e preparacao de dados
################################################################################
## Leitura dos previstos pelo nowcasting de Floripa
f.now.sms <- read.csv("https://raw.githubusercontent.com/geinfosms/cenarios_covid_florianopolis/master/nowcasting/dados/covid_preditos.csv")
f.now.sms$INICIO_SINTOMAS  <-  as.Date(f.now.sms$INICIO_SINTOMAS)

## Leitura e tratamento da planilha de notificações
dados <- read.csv("cenarios_covid_florianopolis/nowcasting/dados/covid_anonimizado_2.csv", encoding = "latin1") 
names(dados) <- c("id", "uf_not",
                  "mun_not", "estrangeiro",
                  "prof_saude", "sexo",
                  "uf_res", "mun_res",
                  "bairro", "dt_not",
                  "dor_garg", "dispneia",
                  "febre", "tosse",
                  "dt_sin_pri", "resul_teste",
                  "unidade_ref", "equipe_ref",
                  "raca", "dt_resul_teste",
                  "tipo_teste", "idade")
## Converte para formato de datas
dt.cols <- names(dados)[grepl("dt_", names(dados))]
## usa lubridate
dados[, dt.cols] <- lapply(dados[, dt.cols],
                             function(x)
                                 as_date(parse_date_time(x, c("dmy", "ymd", "mdy", "dmy HMs", "ymd HMs"))))
## Testes positivos ##
## Filtra residentes em Floripa e testes positivos
dados2 <- dados %>%
    filter(mun_res=="FLORIANOPOLIS" & resul_teste == "POSITIVO")
## datas discrepantes
## Anteriores a 2020 ou posteriorees ao dia de hoje
dados2 %>% filter(dt_resul_teste>Sys.Date() | dt_resul_teste < as.Date("2020-01-01")) %>% select(dt_not, dt_resul_teste)
## Corrigindo
dados2$dt_resul_teste[which(dados2$dt_resul_teste == "1976-04-17")]  <- as.Date("2020-04-17")
dados2$dt_resul_teste[which(dados2$dt_resul_teste == "2020-10-06")]   <- as.Date("2020-06-10")
## Data de exame anterior aos sintomas: muito casos, verificar se é isso mesmo
dados2 %>% filter(dt_resul_teste<dt_sin_pri) %>% select(dt_sin_pri, dt_resul_teste, dt_not)
## Data de notificacao anterior aos sintomas
dados2 %>% filter(dt_not<dt_sin_pri) %>% select(dt_not, dt_sin_pri, dt_resul_teste)
## Há casos com data de exame anterior à notificação?
dados2 %>% filter(dt_resul_teste<dt_sin_pri) %>% select(dt_sin_pri, dt_resul_teste) ## muitos
## Imputacao das datas de resultado faltantes pela mediana do tempo entre sintoma e teste
## diferenca em dias entre data de sintoma e do teste
dados2 %<>% mutate(dif_dias = as.integer(dt_resul_teste - dt_sin_pri),
                   semana = date2week(dt_sin_pri, numeric=TRUE))
## distribuição de diferencas em dias entre data da notificacao e do exame
    ggplot(dados2, aes(dt_sin_pri, dif_dias)) +
    geom_point()
## Regressao quantilica
m1 <- rq(dif_dias ~ dt_sin_pri, data=dados2)
## Imputacao pela diferença mediana entre data de sintoma e de teste em funcao da data de sintoma primario
newdata <- data.frame(dt_sin_pri = dados2$dt_sin_pri[is.na(dados2$dt_resul_teste)])
dados2$dt_resul_teste[is.na(dados2$dt_resul_teste)] <-
    dados2$dt_sin_pri[is.na(dados2$dt_resul_teste)] + predict(m1, newdata = newdata)
## Criando um campo de record date que seja a maior data entre notificacao, resultado do teste e sintoma
dados2 %<>% mutate(dt_record = pmax(dt_resul_teste, dt_not, dt_sin_pri, na.rm=TRUE))


## Casos positivos + suspeitos ##
## Filtra residentes em Floripa e testes positivos
dados3 <- dados %>%
    filter(mun_res=="FLORIANOPOLIS" & (resul_teste == "POSITIVO" | is.na(resul_teste)))
## datas discrepantes
## Anteriores a 2020 ou posteriores ao dia de hoje : nenhuma
dados3 %>% filter(dt_not>Sys.Date() | dt_not < as.Date("2020-01-01")) %>% select(dt_not, dt_sin_pri)
dados3 %>% filter(dt_sin_pri>Sys.Date() | dt_sin_pri < as.Date("2020-01-01")) %>% select(dt_not, dt_sin_pri)
## Data de notificacao anterior aos sintomas: alguns casos
dados3 %>% filter(dt_not<dt_sin_pri) %>% select(dt_not, dt_sin_pri)
## Criando um campo de record date que seja a maior data entre notificacao e sintoma
## (opção conseravdora para que o nowcasting rode com os registros que tenham data sintoma > data notificação)
dados3 %<>% mutate(dt_record = pmax(dt_not, dt_sin_pri, na.rm=TRUE))


################################################################################
## Nowcasting
################################################################################
## Usa Modificacao da funcao NobBS que guarda distribuicao posterior de trajetórias nowcasting

## Casos positivos ##
f.now <- NobBS.posterior(data = dados2,
                         now = max(dados2$dt_sin_pri)-2,
                         onset_date = "dt_sin_pri",
                         report_date = "dt_record",
                         units = "1 day",
                         moving_window = 40
                         )

## Casos positivos + suspeitos ##
f.now2 <- NobBS.posterior(data = dados3,
                         now = max(dados3$dt_sin_pri)-2,
                         onset_date = "dt_sin_pri",
                         report_date = "dt_record",
                         units = "1 day",
                         moving_window = 40
                         )

## O Nowcasting é feito para os últimos 40 dias
## As estapas a seguir junta ao nowcasting o n de casos nos dias anteriores, desde 15/3

## Casos positivos ##
n.sintoma <- dados2 %>%
    group_by(dt_sin_pri) %>%
    summarise(n.casos = n()) %>%
    as.data.frame()
n.sintoma.zoo <- zoo(n.sintoma[,-1], n.sintoma[,1])
now.pred.zoo.original <- zoo(f.now$estimates[,c("estimate", "lower", "upper")], f.now$estimates$onset_date)
now.pred.zoo <- merge(n.casos = n.sintoma.zoo, now.pred.zoo.original)
## Casos positivos + suspeitos
n.sintoma2 <- dados3 %>%
    group_by(dt_sin_pri) %>%
    summarise(n.casos = n()) %>%
    as.data.frame()
n.sintoma.zoo2 <- zoo(n.sintoma2[,-1], n.sintoma2[,1])
now.pred.zoo.original2 <- zoo(f.now2$estimates[,c("estimate", "lower", "upper")], f.now2$estimates$onset_date)
now.pred.zoo2 <- merge(n.casos = n.sintoma.zoo2, now.pred.zoo.original2)

################################################################################
## Proposta de correcao da parte final do nowcasting
################################################################################
## Preliminar, para avaliar a ideia:
## Substitui o nowcasting dos ultimos dias feito com ML por (Nc * p)
## Onde de Nc é o n de casos previstos pelo nowcasting de suspeitos + positivos
## p é a razao nowcasting positivos / N casos suspeitos + positivos
## Junta resultados dos nowcastings num mesmo dataframe
tudo <- merge(
    merge(now.pred.zoo.preenchido,
          now.pred.zoo2, suffixes = c("conf","todos")),
              zoo(f.now.sms[f.now.sms$DADOS=="Nowcasted",-c(1,2)],as.Date(f.now.sms[f.now.sms$DADOS=="Nowcasted",1]))
)
## Corta pela data máxima do nowcasting do positivos + suspeitos
tudo <- window(tudo, end = max( time(now.pred.zoo2[!is.na(now.pred.zoo2$estimate)]) ))
## Calculo de p: razão nowcasting de positivos (SMS, por ML) /  (suspeitos + positivos )
tudo$p <- tudo$MEDIANA_CASOS/tudo$n.casos.todos ## mediana
## Correcao do nowcasting SMS para os ultimos 10 dias
ndias <- 10
index <- (length(tudo$p)-(ndias-1)):length(tudo$p)
## Mediana
tudo$now.cor <- tudo$MEDIANA_CASOS
tudo$now.cor[index] <- tudo$estimate.todos[index]*tudo$p[index]
## Limite inferior: aplica a proporcao ao limite inferior do nowcasting de positivos + suspeitos 
tudo$now.cor.low <- tudo$II_025
tudo$now.cor.low[index] <- tudo$lower.todos[index]*tudo$p[index]
## Limite superior: aplica a proporcao ao limite superior do nowcasting de positivos + suspeitos 
tudo$now.cor.upp <- tudo$II_975
tudo$now.cor.upp[index] <- tudo$upper.todos[index]*tudo$p[index]

################################################################################
## Calculo do R efetivo: falta fazer com nowcasting SMS corrigido
################################################################################
## Preparacao dos dados
## Junta a cada trajetoria da distribuicao porsterior de trajetorias os valores
## de n de casos observados (antes de 40 dias)
now.pred.zoo.preenchido <- merge.zoo(now.pred.zoo, zoo(,seq(start(now.pred.zoo),end(now.pred.zoo),by="day")), all=T)
consolidated <- na.fill(now.pred.zoo.preenchido$n.casos[is.na(now.pred.zoo.preenchido$estimate)],fill = 0)
df.consolidated <- cbind(time(consolidated),
                         as.data.frame(matrix(rep(as.numeric(consolidated),
                                                  dim(f.now$trajectories)[2]-1),
                                              ncol=dim(f.now$trajectories)[2]-1)))
names(df.consolidated) <- names(f.now$trajectories)
trajectories <- rbind(df.consolidated, f.now$trajectories)
## Tidy version das trajetorias
traj.tidy <- gather(trajectories, "simulation", "n.casos", -date)

## Calculo do R efetivo, método de Cori et al, incorporando erro do nowcasting
## (Por meio de sorteio de trajetorias posteriores do nowcasting)
Re.now <- Re.nowcasting(default.R.cori,
                        trajectories,
                        Nsamples = 1000,
                        .parallel = TRUE)
## renomeia variaveis no objeto resultante
names(Re.now$R) <- gsub("\\(R\\)", ".R", names(Re.now$R))
Re.now$R$data.inicio <- trajectories$date[Re.now$R$t_start]
Re.now$R$data.fim <- trajectories$date[Re.now$R$t_end]               

## R efetivo a partir do nowcasting da SMS, metodo da Cori
Re.sms <- Re.com.data(ncasos = f.now.sms$MEDIANA_CASOS[f.now.sms$DADOS=="Nowcasted"],
                          datas = f.now.sms$INICIO_SINTOMAS[f.now.sms$DADOS=="Nowcasted"],
                          delay = 7)



################################################################################
## Graficos
################################################################################
## Nowcastings
## Plot
## Nowcastings casos positivos do observatorio
p1 <-  tudo %>%
    ggplot(aes(Index)) +
    geom_line(aes(y = n.casos.conf, color = "Casos")) +
    geom_line(aes(y = estimate.conf, color = "Nowcasting Observatório")) +
    geom_ribbon(aes(ymin=lower.conf, ymax = upper.conf),
                fill = RColorBrewer::brewer.pal(3, "Set1")[2], alpha = 0.2) +
    scale_x_date(date_labels = "%d/%b") +
    scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")) +
    xlab("Dia do primeiro sintoma") +
    ylab("Número de novos casos") +
    plot.formatos +
    theme(legend.position = c(0.2, 0.8))

## Nowcasting SMS dos casos positivos
p1b <-  tudo %>%
    ggplot(aes(Index)) +
    geom_line(aes(y = n.casos.conf, color = "Casos")) +
    geom_line(aes(y =  MEDIANA_CASOS, color = "Nowcasting SMS")) +
    geom_ribbon(aes(ymin = II_025, ymax = II_975, fill = "Nowcasting SMS"),
                fill = RColorBrewer::brewer.pal(3, "Set1")[3], alpha = 0.2) +
    scale_x_date(date_labels = "%d/%b") +
    scale_color_manual(name = "", values = RColorBrewer::brewer.pal(4, "Set1")) +
    xlab("Dia do primeiro sintoma") +
    ylab("Número de novos casos") +
    plot.formatos +
    theme(legend.position = c(0.2, 0.8))

## Casos confirmados+ suspeitos: observados e nowcasting do observatorio
p1c <-
    tudo %>%
    ggplot(aes(Index)) +
    geom_line(aes(y = n.casos.todos, color = "Casos conf + suspeitos")) +
    geom_line(aes(y =  estimate.todos, color = "Nowcasting conf + suspeitos")) +
    geom_ribbon(aes(ymin = lower.todos, ymax = upper.todos),
                fill = RColorBrewer::brewer.pal(3, "Set1")[2], alpha = 0.2) +
    scale_x_date(date_labels = "%d/%b") +
    scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")) +
    xlab("Dia do primeiro sintoma") +
    ylab("Número de novos casos") +
    plot.formatos +
    theme(legend.position = c(0.2, 0.8))

## R efetivo
## Observatorio
p2 <-  Re.now$R %>%
    ggplot(aes(data.fim)) +
    geom_line(aes(y = Mean.R, color = "Observatório")) +
    geom_ribbon(aes( ymin = Quantile.0.025.R, ymax = Quantile.0.975.R),
                fill = RColorBrewer::brewer.pal(3, "Set1")[1], alpha = 0.2) +
    scale_x_date(date_labels = "%d/%b") +
    xlab("Data final da janela de cálculo do R efetivo") +
    ylab("R efetivo") +
    geom_hline(yintercept = 1, col="red", lty = 2) +
    plot.formatos +
    scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")[1:2]) +
    xlim(min(time(tudo)), max(time(tudo))) +
    theme(legend.position = c(0.1, 0.8))

## Salva imagens dos plots ##

png("nowcasting_R_floripa_observatorio%1d.png", width=600)
## Nowcasting e R efetivo Observatório
grid.arrange(
    p1, p2,
    nrow=2, ncol=1)
## Nowcasting e R efetivo SMS
grid.arrange(
    p1b, p2 %+% Re.sms$R,
    nrow=2, ncol=1)

## Nowcasting Observatório e SMS casos positivos
p1+ geom_line(aes(y =  MEDIANA_CASOS, color = "Nowcasting SMS")) +
    geom_ribbon(aes(ymin = II_025, ymax = II_975, fill = "Nowcasting SMS"),
                fill = RColorBrewer::brewer.pal(3, "Set1")[3], alpha = 0.2)

## Nowcasting SMS e observado e nowcasting de positivos + suspeitos Observatorio
p1c +
    geom_line(aes(y =  MEDIANA_CASOS, color = "Nowcasting SMS")) +
    geom_ribbon(aes(ymin = II_025, ymax = II_975, fill = "Nowcasting SMS"),
                fill = RColorBrewer::brewer.pal(3, "Set1")[3], alpha = 0.2)

## Nowcasting SMS corrigido e observado e nowcasting de positivos + suspeitos Observatorio
p1c +
    geom_line(aes(y =  now.cor, color = "Nowcasting SMS corrigido")) +
    geom_ribbon(aes(ymin = now.cor.low, ymax = now.cor.upp, fill = "Nowcasting SMS corrigido"),
                fill = RColorBrewer::brewer.pal(3, "Set1")[3], alpha = 0.2)

## R efetivo Observatorio e SMS
p2 +
geom_line(data = Re.sms$R, aes(y = Mean.R, col = "SMS")) +
geom_ribbon(data = Re.sms$R, aes( ymin = Quantile.0.025.R, ymax = Quantile.0.975.R),
            fill=RColorBrewer::brewer.pal(3, "Set1")[2], alpha = 0.1)
dev.off()


