library(tidyverse)
library(lubridate)
library(vroom)

## É preciso ter um clone do repo SEADE em ../../../clone_repo_seade_SP
## Na sua cópia local do metarepo central covid digite: git clone git@github.com:seade-R/dados-covid-sp.git clone_repo_seade_SP/

## Cria arquivo csv com hash e data de cada commit
system("cd ../../../dados/seade/; git pull; git log --pretty=format:'%h,%ad' --date=short > ~/work/pesquisa/colaboracoes/ift/corona/covid19br_repos/central_covid/scripts_R_genericos/seade/nowcasting_seade/hashes_seade.csv") ## note que há mais de um commit por data, usar duplicates para pegar a mais recente de cada dia?
hashes <- read.table("hashes_seade.csv", sep=",", header = FALSE)
names(hashes) <- c("hash", "data")
hashes$data <- as.Date(hashes$data)
## Retem o ultimo commit de cada dia
hashes.last <- hashes[!duplicated(hashes$data),] %>% arrange(data)

## Contando n de casos totais a cada dia que o arquivo foi publicado
seade.ncasos <- data.frame(data.publicacao = hashes.last$data, n.linhas = NA, n.casos = NA, n.obitos = NA)

for(i in 1:nrow(hashes.last)){
    comando <- paste0("cd ../../../dados/seade/; git checkout ", hashes.last$hash[i])
    system(comando)
    nome <- dir("../../../dados/seade/data/", pattern = "casos_obitos_doencas_preexistentes.csv", full.names=TRUE)
    if(length(nome)==0)
        next
    seade.ncasos[i, 2:4] <-
        vroom(nome) %>%
        summarise(n.linhas = n(), n.casos = sum(diagnostico_covid19=="CONFIRMADO"), n.obitos = sum(obito)) 
}
##

save.image()
write.csv(seade.ncasos, file= "seade_casos_obitos_doencas_preexistentes_conta_registros_diarios.csv", row.names=FALSE)
