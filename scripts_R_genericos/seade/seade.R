library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)
library(zoo)
library(geofacet)
library(stringr)

source('../../modelo-variante/functions/end.of.epiweek.R')

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
grid_drs2 <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5),
  col = c(5, 4, 3, 2, 5, 4, 1, 2, 6, 4, 3, 5, 7, 3, 5, 5, 4),
  name = c("DRS 08 Franca", "DRS 05 Barretos", "DRS 15 São José do Rio Preto", "DRS 02 Araçatuba", "DRS 13 Ribeirão Preto", "DRS 03 Araraquara", "DRS 11 Presidente Prudente", "DRS 09 Marília", "DRS 14 São João da Boa Vista", "DRS 10 Piracicaba", "DRS 06 Bauru", "DRS 07 Campinas", "DRS 17 Taubaté", "DRS 16 Sorocaba", "DRS 01 Grande SP", "DRS 04 Baixada Santista", "DRS 12 Registro"),
  code = c("DRS 8", "DRS 5", "DRS 15", "DRS 2", "DRS 13", "DRS 3", "DRS 11", "DRS 9", "DRS 14", "DRS 10", "DRS 6", "DRS 7", "DRS 17", "DRS 16", "DRS 1", "DRS 4", "DRS 12"),
  stringsAsFactors = FALSE
)
grid_drs3 <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6),
  col = c(5, 4, 3, 2, 5, 4, 6, 1, 2, 4, 5, 6, 3, 7, 5, 6, 8, 5, 4, 6, 6, 5),
  name = c("DRS 08 Franca", "DRS 05 Barretos", "DRS 15 São José do Rio Preto", "DRS 02 Araçatuba", "DRS 13 Ribeirão Preto", "DRS 03 Araraquara", "DRS 14 São João da Boa Vista", "DRS 11 Presidente Prudente", "DRS 09 Marília", "DRS 10 Piracicaba", "DRS 07 Campinas", "Grande SP Norte", "DRS 06 Bauru", "Grande SP Leste", "Grande SP Oeste", "Município de São Paulo", "DRS 17 Taubaté", "Grande SP Sudoeste", "DRS 16 Sorocaba", "Grande SP Sudeste", "DRS 04 Baixada Santista", "DRS 12 Registro"),
  code = c("DRS 8", "DRS 5", "DRS 15", "DRS 2", "DRS 13", "DRS 3", "DRS 14", "DRS 11", "DRS 9", "DRS 10", "DRS 7", "GSP N", "DRS 6", "GSP L", "GSP O", "MSP", "DRS 17", "GSP SO", "DRS 16", "GSP SE", "DRS 4", "DRS 12"),
  stringsAsFactors = FALSE
)
#geofacet::grid_preview(grid_drs)

# checkout local de https://github.com/seade-R/dados-covid-sp
dir_dados <- "../../dados/seade/data/"

# dados de internações
dados_seade <- read.csv(paste0(dir_dados, "plano_sp_leitos_internacoes.csv"), sep=';', dec=',')
dados_seade$datahora <- as_date(parse_date_time(dados_seade$datahora, "ymd"))

# dados de casos totais
dados_casos <- read.csv(file.path(dir_dados, "dados_covid_sp.csv"), sep=";", dec=",")
dados_casos$datahora <- as.Date(dados$datahora)
dados_casos %>%
    filter(cod_drs != 0) %>%
    mutate(epiweek = end.of.epiweek(datahora)) %>%
    group_by(epiweek, nome_drs) %>%
    summarise(n = sum(casos_novos)) %>%
    as.data.frame() -> dados_casos_drs
# yoy
dados_casos_drs$nome_drs[dados_casos_drs$nome_drs == "DRS 01 Grande São Paulo"] <- "DRS 01 Grande SP"

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

converte.nome.drs <- function(x){
    drs <- c("DRS 02 Araçatuba", "DRS 03 Araraquara", "DRS 04 Baixada Santista",
      "DRS 05 Barretos", "DRS 06 Bauru", "DRS 07 Campinas",
      "DRS 08 Franca", "DRS 09 Marília", "DRS 10 Piracicaba",
      "DRS 11 Presidente Prudente", "DRS 12 Registro", "DRS 13 Ribeirão Preto",
      "DRS 14 São João da Boa Vista", "DRS 15 São José do Rio Preto", "DRS 16 Sorocaba",
      "DRS 17 Taubaté", "DRS 01 Grande São Paulo")
    drs.nome <- substr(drs, 8, stop=100)
    return(drs[which(x == drs.nome)])
}

caption <- paste0("fonte: Seade ", format(Sys.Date(), "%d/%m/%Y"),
                  ". Crédito: Observatório Covid-19 BR")

p_casos_drs <- dados_casos_drs %>%
    filter(epiweek < end.of.epiweek(Sys.Date())) %>%
    ggplot() +
    geom_line(aes(x=epiweek, y=n)) +
    scale_x_date(date_breaks = "2 month", date_labels = "%b") +
    facet_geo(~nome_drs, grid=grid_drs2, scales="free") +
    labs(x = "data de notificação", y = "casos totais",
         caption=caption)

dadossp <- filtra.drs(dados_seade, "01")
p <- ggplot(dadossp) +
    geom_line(aes(x=datahora, y=internacoes_7d)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x="data de notificação",
         y="internações nos últimos 7 dias",
         title="DRS 01 Grande São Paulo",
         caption=caption)

p_tudo <- dados_seade %>%
    filter(nome_drs == "Estado de São Paulo") %>%
    ggplot() +
    geom_line(aes(x=datahora, y=internacoes_7d)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x="data de notificação",
         y="internações nos últimos 7 dias",
         title="Estado de São Paulo",
         caption=caption)

p_estado <- ggplot(dados_seade) +
    geom_line(aes(x=datahora, y=internacoes_7d)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x="data de notificação",
         y="internações nos últimos 7 dias",
         title="Internações nos últimos 7 dias",
         caption=caption) +
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
         caption=caption) +
    facet_geo(~nome_drs, grid=grid_drs2, scales="free")

p_estado3 <- dados_seade %>%
    filter(nome_drs != "Estado de São Paulo") %>%
    ggplot() +
    geom_line(aes(x=datahora, y=internacoes_7d)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b") +
    labs(x="data de notificação",
         y="internações nos últimos 7 dias",
         title="Internações nos últimos 7 dias",
         caption=caption) +
    facet_geo(~nome_drs, grid=grid_drs3, scales="free")


## Ocupação
png("ocupacao_UTI.png", width =800)
    dados_seade %>%
    filter(nome_drs != "Estado de São Paulo") %>%
    ggplot() +
    geom_line(aes(x=datahora, y=ocupacao_leitos)) +
    scale_x_date(date_breaks = "3 month", date_labels = "%b") +
    labs(x="data de notificação",
         y="Ocupação de leitos UTI %",
         title="Ocupação de UTIS destinadas a COVID",
         caption=caption) +
    ylim(c(0,100)) +
    facet_geo(~nome_drs, grid=grid_drs3)
dev.off()


## N internaçãoes em  UTI
## No repo do SEADe está
## PI: "Média móvel para 7 dias do Pacientes Internados em Leitos de UTI Destinados para COVID-19 no dia". Me parece ser n de internados em cada dia, não n de novas internações. Mas não é 100% claro, né. Verificar em alguma documentação?
png("internados_UTI.png", width =800)
    dados_seade %>%
    filter(nome_drs != "Estado de São Paulo") %>%
    ggplot() +
    geom_line(aes(x=datahora, y=pacientes_uti_mm7d)) +
    scale_x_date(date_breaks = "3 month", date_labels = "%b") +
    labs(x="",
         y="Média móvel internados em UTI",
         title="Média móvel de 7 dias de internados em UTI",
         caption=caption) +
    facet_geo(~nome_drs, grid=grid_drs3, scale = "free")
dev.off()
