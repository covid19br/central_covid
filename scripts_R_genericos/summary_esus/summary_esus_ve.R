######lendo uma base por vez#####

library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(R.utils)
library(readr)

###parametros###

########summary da e-sus ve########################

######TATIANA
nome.dir="C:/Users/Tatiana/Documents/analises_covid/teste"
setwd(nome.dir)
estado.name="santa catarina"
mun.name="florian?polis"
file.names<-list.files(path=nome.dir, pattern="esus-ve_2020_")
get.data.base(file.names)
########

###ANDREA
#eu estou em central covid e tenho o pacote now_fcts dentro da pasta:
#daqui now_fct fica em ./now_fct/ por isso faço:
devtools::load_all("./now_fcts/")
#para ter as funções à mão

#daqui os dados ficam em ./dados/ entao pegar só santa catarina seria tipo:
#de todas as bases de sc
file.names <- list.files(path = "./dados/eSUS-VE/",
                         pattern = "sc", full.names = TRUE)
#####
estado.name = "santa catarina"
mun.name = "Florianópolis"
geocode_cidade <- get.geocode(nome_municipio = mun.name, sigla = "SC")

# o vetor inteiro de datas:
datas.base <- get.data.base(file.names)

#######calcula numero de casos por data de primeiros sintomas e por data da base#####

#isto tem que virar uma funcao e tem que checar se uma base não foi já processada para não repetir
for (i in 1:length(file.names)) {
  file.name <- file.names[i]
  esus_ve <- read.esus.generica(file.name = file.name)
#se der erro a coluna estado vem ruim, melhor filtrar sempre:
  esus_ve <- esus_ve %>% filter(tolower(estado) == estado.name)
  data.base <- datas.base[i]
###   nem todas as bases têm geocodes.
  if ("municipioibge" %in% names(esus_ve)) {
    cidade <- esus_ve %>%
    #filter(tolower(estado) == estado.name) %>%
     filter(tolower(municipioibge) == geocode_cidade)
}
  if ("municipio" %in% names(esus_ve)) {
    cidade <- esus_ve %>%
      filter(municipio == mun.name)
  }
  dados <- filtros.esusve(cidade, tipo = "covid") %>%
    select(datainiciosintomas)
  dados2 <- dados %>%
    group_by(datainiciosintomas) %>%
  summarise(n.casos = n())  %>%
  mutate(data.base = data.base) %>%
  as.data.frame()
  #escreve um arquivo por data
  #newFileName <-  paste0("./scripts_R_genericos/summary_esus/Florianopolis/summary_esus_", mun.name, "_last_date_",data.base ,".csv")
  #write_csv(dados2, newFileName, col_names = )#comentei aqui mas vai criar um csv por data
  #escreve um arquivo por municipio já juntando todas as datas (é redundante com bind_rows)
  munFileName <-  paste0("./scripts_R_genericos/summary_esus/Florianopolis/summary_esus_", mun.name,".csv")
  write_csv(dados2, path = munFileName, append = TRUE)
}
