library(plyr)
library(dplyr)
library(vroom)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(zip)
library(aweek)
library(lubridate)
library(rmarkdown)
source("../../nowcasting/fct/get.last.date.R")
source("functions.R")
set_week_start("Sunday")


################################################################################
## Dados do Infogripe sem filtros
################################################################################
## Leitura 
## infogripe <- read.csv2("https://gitlab.procc.fiocruz.br/mave/repo/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_sintomas.csv")
infogripe <- vroom("https://gitlab.fiocruz.br/marcelo.gomes/infogripe/-/raw/master/Dados/InfoGripe/serie_temporal_com_estimativas_recentes_sem_filtro_sintomas.csv.gz",  delim=";" , locale=locale(decimal_mark=',')) %>%
    as.data.frame()

##infogripe$data.de.publicação <- as.Date(infogripe$data.de.publicação)
infogr.last.date <- format(as.Date(max(infogripe[,"data de publicação"])), format = "%Y_%m_%d")
print(paste('data nowcasting InfoGripe:', infogr.last.date))

## Filtra apenas os dados de incidência agregados para SP
## Verificando dados de n de casos e incidencia:
infogripe %>%
    filter(Tipo == "Estado" &
           `Unidade da Federação` == "São Paulo",
           `Ano epidemiológico` > 2019 #&
           ## data.de.publicação == max(data.de.publicação) &
           ##& dado == "srag" &
           ##escala == "incidência"
           ) %>%
    group_by(dado, escala) %>%
    summarise(nmaximo = max(`Casos semanais reportados até a última atualização`, na.rm=TRUE))
    
## Cria o objeto de dados
infogr.SP <-
    infogripe %>%
    filter(Tipo == "Estado" &
           `Unidade da Federação` == "São Paulo" &
           `Ano epidemiológico` > 2019 #&
           ## data.de.publicação == max(data.de.publicação) &
           ) %>%
    select( 
        dado,
        escala,
        `Ano epidemiológico`,
        `Semana epidemiológica`,
        `Casos semanais reportados até a última atualização`,
        `limite inferior da estimativa`,
        `casos estimados`,
        `limite superior da estimativa`) %>%
    dplyr::rename(
               ano = `Ano epidemiológico`,
               semana = `Semana epidemiológica`,
               casos.obs = `Casos semanais reportados até a última atualização`,
               lower = `limite inferior da estimativa`,
               casos.est = `casos estimados`,
               upper = `limite superior da estimativa`
               )
    
infogr.SP$data <- week2date(epi.data(infogr.SP$semana, infogr.SP$ano))

## Graficos
theme_Publication <- function(base_size=14, base_family="helvetica") {
    (theme_foundation(base_size=base_size, base_family=base_family)
        + theme(plot.title = element_text(face = "bold",
                                          size = rel(1.2), hjust = 0.5),
                text = element_text(),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                panel.border = element_rect(colour = NA),
                axis.title = element_text(face = "bold",size = rel(1)),
                axis.title.y = element_text(angle=90,vjust =2),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(), 
                axis.line = element_line(colour="black"),
                axis.ticks = element_line(),
                panel.grid.major = element_line(colour="#f0f0f0"),
                panel.grid.minor = element_blank(),
                legend.key = element_rect(colour = NA),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.key.size= unit(0.2, "cm"),
                ##legend.margin = unit(0, "cm"),
                legend.spacing = unit(0.2, "cm"),
                legend.title = element_text(face="italic"),
                plot.margin=unit(c(10,5,5,5),"mm"),
                strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                strip.text = element_text(face="bold")
                ))
    
}


## SRAG COVID
p1 <-
    infogr.SP %>%
    filter(dado == "sragcovid" & escala == "incidência") %>%
    ggplot(aes(data, casos.obs)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax = upper), fill="red", alpha =0.5)+
    scale_x_date(date_labels = "%b/%Y") +
    xlab("")+
    theme_Publication()

## SRAG COVID
## Incidência
p1 %+% filter(infogr.SP, dado == "sragcovid" & escala == "incidência") +
    ggtitle("Estado de SP, casos de SRAG confirmados para COVID") +
    ylab("Incidência por 100 mil habitantes") 
ggsave("output/SP_SRAG_covid_incidencia.png", width = 8, height = 4.5)
## Casos
p1 %+% filter(infogr.SP, dado == "sragcovid" & escala == "casos") +
    ggtitle("Estado de SP, casos de SRAG confirmados para COVID") +
    ylab("Número de casos") 
ggsave("output/SP_SRAG_covid_ncasos.png", width = 8, height = 4.5)

## Obitos
## Incidência
p1 %+% filter(infogr.SP, dado == "obitocovid" & escala == "incidência") +
    ggtitle("Estado de SP, óbitos confirmados para COVID") +
    ylab("Incidência por 100 mil habitantes") 
ggsave("output/SP_obitos_covid_incidencia.png", width = 8, height = 4.5)
## Casos
p1 %+% filter(infogr.SP, dado == "obitocovid" & escala == "casos") +
    ggtitle("Estado de SP, óbitos confirmados para COVID") +
    ylab("Número de casos") 

ggsave("output/SP_obitos_covid_ncasos.png", width = 8, height = 4.5)

