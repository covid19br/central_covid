source("./analise_UOL/load.R")
PRJROOT <- rprojroot::find_root(".here")

# sources functions
devtools::load_all("./now_fcts/") ##loading de funções necessárias##

dir<-"../../Downloads/esus/"
files <- list.files(dir)
pforeach{()
  dados <-  read.esus.generica(paste0(dir, files,".csv"))
  dados %>% 
    filter(resultadoteste == "Positivo" | classificacaofinal == "Confirma") %>% 
    group_by(datainiciosintomas) %>% 
    summarize(N=n()) %>% 
    as.data.frame()
  substr(files, 6, 20L)
}



  