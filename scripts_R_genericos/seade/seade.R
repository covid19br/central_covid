library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)
library(zoo)

Sys.setlocale("LC_ALL","pt_BR.utf8")

# checkout local de https://github.com/seade-R/dados-covid-sp
dir_dados <- "../../dados/seade/data/"

dados_seade <- read.csv(paste0(dir_dados, "plano_sp_leitos_internacoes.csv"), sep=';', dec=',')
dados_seade$datahora <- as_date(parse_date_time(dados_seade$datahora, "ymd"))

# recebe nome 
filtra.drs <- function(dados, drs){
    pattern <- drs
    if (grepl("01", drs))
        pattern <- "Município de São Paulo|Grande SP"
    dados <- dados %>%
        filter(grepl(pattern, nome_drs)) %>%
        as.data.frame()
    if (grepl("01", drs)){
        dados <- dados %>%
            group_by(datahora) %>%
            summarise(pacientes_uti_mm7d = sum(pacientes_uti_mm7d),
                      total_covid_uti_mm7d = sum(total_covid_uti_mm7d),
                      pop = sum(pop),
                      internacoes_7d = sum(internacoes_7d),
                      internacoes_7d_l = sum(internacoes_7d_l)) %>%
            as.data.frame()
    }
    return(dados)
}

dadossp <- filtra.drs(dados_seade, "01")
p <- ggplot(dadossp) +
    geom_line(aes(x=datahora, y=internacoes_7d)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x="data de notificação",
         y="internações nos últimos 7 dias",
         title="DRS 01 Grande São Paulo",
         caption="fonte: Seade")
#ggsave('internacoes7d.png', width=8, height=6)

p_estado <- ggplot(dados_seade) +
    geom_line(aes(x=datahora, y=internacoes_7d)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x="data de notificação",
         y="internações nos últimos 7 dias",
         title="Internações nos últimos 7 dias",
         caption="fonte: Seade") +
    facet_wrap(~nome_drs, scales="free")

p_ufabc <- dados_seade %>%
    filter(nome_drs %in% c("Município de São Paulo", "Grande SP Sudeste")) %>%
    ggplot() +
    geom_line(aes(x=datahora, y=internacoes_7d)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x="data de notificação",
         y="internações nos últimos 7 dias",
         title="Internações nos últimos 7 dias",
         caption="fonte: Seade. Grande SP Sudeste compreende Diadema, Mauá, Ribeirão Pires,\nRio Grande da Serra, Santo André, São Bernardo do Campo e São Caetano do Sul") +
    facet_wrap(~nome_drs, scales="free", nrow=2)

