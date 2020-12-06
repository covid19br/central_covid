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
