#-------------------------------------------------------
# Script para gerar tabela consolidada por DRS
# pega tabelas de Re, casos e obitos COVID mais recente
#-------------------------------------------------------

library(dplyr)
library(data.table)
library(textclean)
library(nowfcts)
library(tools)

## funcao para facilitar juntar os dados para a data mais recente
get.max.values <- function(tipo, path){
  drs.path <- list.dirs(path, recursive = FALSE)
  drs.name <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  data.max <- sapply(drs.path, function(x) get.last.date(paste0(x, "/tabelas_nowcasting_para_grafico")))
  drs.val <- list()
  for (i in 1:length(drs.name)) {
    drs.val[[i]] <- list.files(path = paste0(drs.path[i], "/tabelas_nowcasting_para_grafico"),
                               pattern = paste0(tipo, data.max[i], "*.csv"),
                               full.names = TRUE)
  }
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
  df.data <-  data.frame(data.base.sivep = data.max, DRS.nome.nonascii = drs.name)
  df.res <- left_join(df.drs, df.data, by = "DRS.nome.nonascii")

  return(df.res)
}


# info DRS e municipios ---------------------------------------------------
drs.df <- read.csv("../dados/DRS_com_coordenadas.csv", dec = ",")
drs.df$DRS.nome.nonascii <- replace_non_ascii(gsub(" ", "_", drs.df$DRS))

# pegando os dados mais recentes ---------------------------------------------------------------
## usa a funcao get.max.values
vals.path <- "../site/dados/DRS/SP"
## guarda data max
data.max <- get.last.date(list.dirs(vals.path)[3])

## r efetivo
re.drs <- get.max.values(tipo = "r_efetivo_covid_", path = vals.path) %>%
  select(DRS.nome.nonascii, R.efetivo = Mean.R)
## casos
caso.drs <- get.max.values("nowcasting_acumulado_covid_", path = vals.path) %>%
  select(DRS.nome.nonascii, casos = not.mean.c)
## obitos
obito.drs <- get.max.values("nowcasting_acumulado_obitos_covid_", path = vals.path)  %>%
  select(DRS.nome.nonascii, obitos = not.mean.c, data.base.sivep)

# juntando todos os dados em uma unica tabela
dados.drs <- left_join(drs.df, re.drs) %>%
  left_join(caso.drs) %>%
  left_join(obito.drs) %>%
  select(-DRS.nome.nonascii) %>%
  rename(óbitos = obitos)

names(dados.drs) <- toTitleCase(gsub(".", " ", names(dados.drs), fixed = TRUE))

dados.drs

write.csv(dados.drs, "../dados_processados/tabelas_consolidadas_divulgacao/dados_DRS_SP_para_mapa.csv",
          row.names = FALSE)
