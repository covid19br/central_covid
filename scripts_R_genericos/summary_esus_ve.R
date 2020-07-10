######lendo uma base por vez#####

library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(R.utils)
library(readr)

###parâmetros###

########summary da e-sus ve########################

########parâmetros#####

nome.dir="C:/Users/Tatiana/Documents/analises_covid/teste"
setwd(nome.dir)
estado.name="santa catarina"
mun.name="florianópolis"
file.names<-list.files(path=nome.dir, pattern="esus-ve_2020_")
get.data.base(file.names)

#######calcula número de casos por data de primeiros sintomas e por data da base#####

for (file.name in file.names) {
  esus_ve<-read_delim(file = file.name,
                      delim = ";",
                      escape_double = FALSE,
                      trim_ws = TRUE,
                      locale = readr::locale(encoding = "UTF-8"))
  index_nada <- problems(esus_ve)$row   #retirar essa linha depois de incorporar o read.esus ve
  esus_ve <- esus_ve[-index_nada,]      # retirar essa linha depois de incorporar o read.esus ve
  cidade<- esus_ve %>% filter (tolower(estado)==estado.name) %>%
                       filter (tolower(municipio)==mun.name)
  cidade$dataInicioSintomas<-substr(cidade$dataInicioSintomas,1,10) ####tirar essa linha depois de incorporar o read esus ve
  cidade$dataInicioSintomas<-ymd(cidade$dataInicioSintomas)  ###tirar essa linha depois de incorporar o read esus-ve
  dados<- cidade %>%  filter (resultadoTeste=="Positivo" | str_detect(classificacaoFinal, "Confirma")) %>%
                     select (dataInicioSintomas) 
  dados2<- dados %>% group_by(dataInicioSintomas) %>%
  summarise(Casos=n())  %>%                              
  as.data.frame()
  data.base<-get.data.base(file.name)
  dados2$data.base<-data.base
  newFileName <-  paste0("summary_esus_", mun.name, data.base ,".csv")
  write.csv(dados2, newFileName, row.names = FALSE)
}









