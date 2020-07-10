######lendo uma base por vez#####

library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(R.utils)
library(readr)

###parâmetros###

####em desenvolvimento###

nome.dir="C:/Users/Tatiana/Documents/analises_covid/teste"
setwd(nome.dir)
estado.name="santa catarina"
mun.name="florianópolis"
file.names<-list.files(path=nome.dir, pattern="esus-ve_2020_")

####fazer um loop pra isso se repetir com todas as bases####

for (file.name in file.names) {
  esus_ve<-read_delim(file = file.name,
                      delim = ";",
                      escape_double = FALSE,
                      trim_ws = TRUE,
                      locale = readr::locale(encoding = "UTF-8"))
  index_nada <- problems(esus_ve)$row   #retirar essa linha depois de incorporar o read.esus ve
  esus_ve <- esus_ve[-index_nada,]      # retirar essa linha depois de incorporar o read.esus ve
  data.base<-substr(file.name,7,17)
  cidade<- esus_ve %>% filter (tolower(estado)==estado.name) %>%
                       filter (tolower(municipio)==mun.name)
  cidade$dataSintomas<-substr(cidade$dataInicioSintomas,1,10) ####tirar essa linha depois de incorporar o read esus ve
  cidade$dataSintomas<-ymd(cidade$dataInicioSintomas)  ###tirar essa linha depois de incorporar o read esus-ve
  dados<- cidade %>%  filter (resultadoTeste=="Positivo" | str_detect(classificacaoFinal, "Confirma")) %>%
                     select (dataInicioSintomas) 
  dados2<- dados %>% group_by(dataInicioSintomas) %>%
  summarise(data.base=n())  %>%                              
  as.data.frame()
  print(dados2)
 }


#write.table(dados2,file.name, row.names=FALSE )










