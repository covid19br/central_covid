library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)
library(zoo)
library(geofacet)

Sys.setlocale("LC_ALL","pt_BR.utf8")

grid_drs <- data.frame(
  row = c(1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5),
  col = c(5, 4, 5, 2, 3, 6, 4, 1, 2, 3, 5, 4, 6, 4, 5, 4, 5),
  name = c("DRS 08 Franca", "DRS 05 Barretos", "DRS 13 Ribeirão Preto",
           "DRS 02 Araçatuba", "DRS 15 São José do Rio Preto", 
           "DRS 14 São João da Boa Vista", "DRS 03 Araraquara",
           "DRS 11 Presidente Prudente", "DRS 09 Marília", "DRS 06 Bauru", 
           "DRS 07 Campinas", "DRS 10 Piracicaba", "DRS 17 Taubaté",
           "DRS 16 Sorocaba", "DRS 01 Grande SP", "DRS 12 Registro",
           "DRS 04 Baixada Santista"),
  code = c("DRS 8", "DRS 5", "DRS 13", "DRS 2", "DRS 15", "DRS 14", "DRS 3",
           "DRS 11", "DRS 9", "DRS 6", "DRS 7", "DRS 10", "DRS 17", "DRS 16",
           "DRS 1", "DRS 12", "DRS 4"),
  stringsAsFactors = FALSE
)
#geofacet::grid_preview(grid_drs)

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

dadossp$nome_drs <- "DRS 01 Grande SP"
common_cols <- intersect(colnames(dadossp), colnames(dados_seade))
dados_estado <- rbind(dados_seade[,common_cols], dadossp[,common_cols])
dados_estado <- dados_estado %>% filter(grepl("DRS ", nome_drs))

p_estado2 <- ggplot(dados_estado) +
    geom_line(aes(x=datahora, y=internacoes_7d)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b") +
    labs(x="data de notificação",
         y="internações nos últimos 7 dias",
         title="Internações nos últimos 7 dias",
         caption="fonte: Seade") +
    facet_geo(~nome_drs, grid=grid_drs, scales="free")


