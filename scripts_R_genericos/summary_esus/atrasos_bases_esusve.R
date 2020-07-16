
library(tibble)
library(reshape)
library(tidyr)
install.packages("splitstackshape")
library(splitstackshape)

munFileName <- "summary_esus_Florianopolis_v3.csv"
tudo<-read.csv(munFileName, sep=";")
names(tudo)<-c("datainiciosintomas", "n.casos", "data.base")

###código leitura Andréia######

tudo <- read.csv(munFileName) %>%
  arrange(data.base, datainiciosintomas) %>%
  filter(!is.na(datainiciosintomas))

data_wide <- tidyr::pivot_wider(tudo, names_from = data.base, values_from = n.casos) %>% data.frame()

names(data_wide) <- str_remove(string = names(data_wide),pattern = "X")

row.names(data_wide)<-data_wide$datainiciosintomas
data_wide<-data_wide[,-1]
data_wide[is.na(data_wide)]<-0

##########calculando a diferença entre as bases - Tati - adaptação do método Rafa #######

##aqui calcula a diferença entre os casos acumulados por data de sintoma de uma base para outra. 
#Isso gera um novo data.frame com número de casos novos por data de sintoma e data de base

nc <- ncol(data_wide)
d <- data_wide[-1] - data_wide[-nc]

####aqui gera um novo data frame com data de sintoma e data de notificação para cada caso

d<-tibble:: rownames_to_column(d,"dataInicioSintomas")
table2<- reshape::melt (d, "dataInicioSintomas")
table3<-expandRows(table2,"value")   ####não está dando certo porque tem valor negativo 

####tem que ver como conferir e o que fazer com os valores negativos####










