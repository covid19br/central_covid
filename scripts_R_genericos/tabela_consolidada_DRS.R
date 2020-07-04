#library(tmap)
library(dplyr)
#library(geobr)
library(data.table)
#library(maptools)
#remotes::install_github("covid19br/now_fcts", ref = 'plot')
library(nowfcts)

## funcao para facilitar juntar os dados para a data mais recente
get.max.values <- function(tipo, path){
  drs.path <- list.dirs(path, recursive = FALSE)
  drs.name <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  data.max <- get.last.date(paste0(drs.path[1], "/tabelas_nowcasting_para_grafico"))
  drs.val <- lapply(drs.path, function(x) list.files(path = paste0(x, "/tabelas_nowcasting_para_grafico"),
                                                     pattern = paste0(tipo, data.max, "*.csv"),
                                                     full.names = TRUE))
  names(drs.val) <- drs.name
  ## df com Re para todas as DRS
  df.drs <- bind_rows(lapply(drs.val, fread), .id = "DRS.nome.nonascii")
  if (tipo == "r_efetivo_covid_") {
    df.drs <- df.drs %>%
      group_by(DRS.nome.nonascii) %>%
      filter(data == max(data, na.rm = TRUE))
  } else {
    df.drs <- df.drs %>%
      group_by(DRS.nome.nonascii) %>%
      filter(!is.na(not.mean.c)) %>%
      filter(data == max(data, na.rm = TRUE))
  }

  return(df.drs)
}


data.max <- get.last.date(list.dirs(vals.path)[3])

# info DRS e municipios ---------------------------------------------------
drs <- read.csv("../site/dados/DRS_SP.csv")
drs.df <- distinct(drs[, c("DRS", "DRS.nome.nonascii")])

# pegando os dados mais recentes ---------------------------------------------------------------
## usa a funcao get.max.values
vals.path <- "../site/dados/DRS/SP"

## r efetivo
re.drs <- get.max.values(tipo = "r_efetivo_covid_", path = vals.path) %>%
  select(DRS.nome.nonascii, R.efetivo = Mean.R)
## casos
caso.drs <- get.max.values("nowcasting_acumulado_covid_", path = vals.path) %>%
  select(DRS.nome.nonascii, casos = not.mean.c)
## obitos
obito.drs <- get.max.values("nowcasting_acumulado_obitos_covid_", path = vals.path)  %>%
  select(DRS.nome.nonascii, obitos = not.mean.c)

dados.drs <- left_join(drs.df, re.drs) %>%
  left_join(caso.drs) %>%
  left_join(obito.drs)

head(dados.drs)
