library(dplyr)
library(RSQLite)
library(lubridate)
library(parallel)


## Importa csvs do esus para uma base SQLite
add.to.db <- function(file.names, db.name, table.name, n.cores = 6, parallel = FALSE, ...){
    db <- dbConnect(SQLite(), dbname = db.name)
    if(parallel){
        tmp <- bind_rows(
            mclapply(file.names, read.esus.generica,
                     mc.cores = min(n.cores, length(file.names)),
                     convert.dates = FALSE))
            dbWriteTable(db, table.name, tmp, ...)
    }
    else{
        tmp <- read.esus.generica(file.names[1], convert.dates=FALSE)
        dbWriteTable(db, table.name, tmp, overwrite = TRUE)
        if(length(file.names>1)){
            for(i in 2:length(file.names)){
                tmp <- read.esus.generica(file.names[i], convert.dates=FALSE)
                dbWriteTable(db, table.name, tmp, append = TRUE)
                }
            }
        }
    dbDisconnect(db)
}

## Convert many date formats
date.conv <- function(x){
    as_date(parse_date_time(x, c("dmy", "ymd", "mdy", "dmy HMs", "ymd HMs")))
}

## Lê uma tabela na base de dados e conta casos por data de sintoma, estado, estado do teste, tipo de tese, classificação final e resultado do teste
sumario.1 <-  function(tabela, db = my.db){
    tbl(db, tabela) %>%
        select(municipioIBGE, estadoIBGE, dataInicioSintomas, estadoTeste, tipoTeste, resultadoTeste, classificacaoFinal) %>%
        mutate(estado = ifelse(!is.na(municipioIBGE)&is.na(estadoIBGE), substr(municipioIBGE,1,2), estadoIBGE)) %>%
        group_by(dataInicioSintomas, estado, estadoTeste, tipoTeste, classificacaoFinal, resultadoTeste) %>%
        summarise(N=n()) %>%
        data.frame()
}

sumario.2 <- function(tabela, db = my.db){
    tbl(db, tabela) %>%
        select(dataInicioSintomas, estadoIBGE, municipioIBGE,
               estadoTeste, tipoTeste, resultadoTeste) %>%
        filter(
            tipoTeste == "RT-PCR" &
            estadoTeste!="Exame Não Solicitado" &
            !is.na(estadoTeste) &
            !is.na(dataInicioSintomas)) %>%
    mutate(estado = ifelse(!is.na(municipioIBGE)&is.na(estadoIBGE), substr(municipioIBGE,1,2), estadoIBGE)) %>%
        group_by(estado, dataInicioSintomas, resultadoTeste) %>%
        summarise(N = n()) %>%
        data.frame() %>%
        mutate(dataInicioSintomas = as.Date(dataInicioSintomas)) %>%
        filter(dataInicioSintomas > as.Date("2020-01-01") &
               dataInicioSintomas <= Sys.Date()) %>%
        mutate(sem_sin = week2date(date2week(dataInicioSintomas, floor_day = TRUE)),
               resultadoTeste = ifelse(resultadoTeste=="null"|is.na(resultadoTeste), "Em branco (null + NA)", resultadoTeste))
}
