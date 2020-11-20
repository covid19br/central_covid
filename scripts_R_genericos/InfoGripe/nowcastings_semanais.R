library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(zip)
library(aweek)
library(rmarkdown)
source("../../nowcasting/fct/get.last.date.R")
source("../../nowcasting/fct/read.sivep.R")
source("../../nowcasting/fct/gera.nowcasting.R")
source("../../nowcasting/fct/00_load_libraries.R")
## set_week_start("Sunday") ## inicio das semanas epidemiológicas


data.dir <- "../../central_covid/dados/SIVEP-Gripe"
data <- get.last.date(data.dir)


## Exemplos: MG e RS
dados.MG <- read.sivep(dir = data.dir, escala = "estado", geocode = "31", sigla = "MG", data= data)
MG.now <- gera.nowcasting(dados.MG, tipo= "srag", trim.now = 0, window = 8, semanal =TRUE)
dados.RS <- read.sivep(dir = data.dir, escala = "estado", geocode = "43", sigla = "RS", data= data)
RS.now <- gera.nowcasting(dados.RS, tipo= "srag", trim.now = 0, window = 8, semanal =TRUE)


## Funcao semvê para plotar
p1 <-function(objeto){
    objeto$dados %>%
        group_by(dt_sin_pri) %>%
        summarise(N = n()) %>%
        ggplot(aes(dt_sin_pri, N)) +
        geom_line() +
        geom_line(data = objeto$now$estimates, aes(x=onset_date, y=estimate), col ="blue") +
        geom_ribbon(data = objeto$now$estimates, aes(x= onset_date, y = estimate, ymin=lower, ymax = upper),
                    fill = "blue", alpha =0.2) +
        theme_bw()
}
png("output/testes_semanal_MG.png")
p1(MG.now)
dev.off()
png("output/testes_semanal_RS.png")
p1(RS.now)
dev.off()
