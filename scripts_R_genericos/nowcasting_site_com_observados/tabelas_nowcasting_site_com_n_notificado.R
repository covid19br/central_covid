## Produz tabelas com os estimados pelo nowcasting do site , e os valores de casos notificados

library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(aweek)
source("../../nowcasting/fct/get.last.date.R")
source("../../nowcasting/fct/makeTabNowObs.R")
set_week_start("Sunday")

## Atualiza o clone do repo de dados seade
system("cd ../../dados/seade/; git pull")

################################################################################
## SEADE: Tabela com codigos municipios, DRS e populacao
################################################################################
## Tabela das DRS e suas populacoes
dic.drs <-
    ## Leitura do arquivo com os dados de notificacao, que tem os dados dos municipios
    read.csv2("../../dados/seade/data/dados_covid_sp.csv") %>% 
    mutate(datahora = as.Date(datahora)) %>%
    ## Seleciona as varaveis para usar na tabela de muncipios e retém apenas um registro por município
    select(nome_munic,codigo_ibge,nome_ra,cod_ra,nome_drs,cod_drs,pop,pop_60,area,latitude,longitude) %>%
    filter(nome_munic != "Ignorado") %>%
    distinct() %>%
    group_by(nome_drs, cod_drs) %>%
    summarise(pop = sum(pop), pop_60 = sum(pop_60), area = sum(area))


################################################################################
## Por DRS de SP
################################################################################

## Nomes das DRS e dos diretorios
DRS <-
    read.csv("../../nowcasting/dados/DRS_SP.csv") %>%
    select(DRS,DRS.nome, DRS.nome.nonascii) %>%
    distinct() %>%
    arrange(DRS) %>%
    data.frame()

## Casos de covid
f1 <- function(DRS)
    makeTabNowObs(diretorio = paste0("../../site/dados/DRS/SP/",DRS,"/tabelas_nowcasting_para_grafico/"),
                  tipo = "covid")
nomes <- DRS$DRS.nome.nonascii
names(nomes) <- DRS$DRS.nome
covid.drs <- ldply(nomes, f1)
names(covid.drs)[1] <- "DRS"
covid.drs$week <- date2week(covid.drs$data, floor_day=TRUE)

## Óbitos por covid
f2 <- function(DRS)
    makeTabNowObs(diretorio = paste0("../../site/dados/DRS/SP/",DRS,"/tabelas_nowcasting_para_grafico/"),
                  tipo = "covid", caso=FALSE)
covid.ob.drs <- ldply(nomes, f2)
names(covid.ob.drs)[1] <- "DRS"
covid.ob.drs$week <- date2week(covid.ob.drs$data, floor_day=TRUE)

## Planilhas
write.csv(covid.drs, file = "outputs/srag_confirmado_por_drs_com_nowcasting.csv")
write.csv(covid.ob.drs, file = "outputs/obitos_srag_confirmado_por_drs_com_nowcasting.csv")

## Graficos com nowcasting e N notificado a cada semana de primeiro sintoma
for(i in 1:length(nomes)){
    png(paste0("outputs/srag_conf_por_semana_com_nowcasting_",nomes[i],".png"))
    p1 <-
        covid.drs %>%
        filter(DRS == names(nomes)[i]) %>%
        ggplot(aes(data)) +
        geom_line(aes(y = n.casos, col = "Notificados"),size = 1.25) +
        geom_line(aes(y=estimate, col = "Nowcasting"), size = 1.25) +
        geom_ribbon(aes(ymin=lower, ymax = upper), fill = "darkred", alpha=0.25) +
        xlab("Último dia da semana epidemiológica") +
        ylab("Casos internados confirmados") +
        theme_bw() +
        theme(legend.position = c(0.15, 0.9),
              legend.title = element_blank(),
              axis.text=element_text(size=14),
              axis.title=element_text(size=15),
              plot.title = element_text(size=16, face="bold"))  +
        scale_color_manual(values = c("darkblue", "darkred")) +
        ggtitle(names(nomes)[i])
    print(p1)
    dev.off()
}
