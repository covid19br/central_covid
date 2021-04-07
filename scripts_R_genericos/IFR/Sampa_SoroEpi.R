library(ggplot2)
library(readr)
library(knitr)
source("functions.R")
source("../../nowcasting/fct/read.sivep.R")

################################################################################
## Dados populacionais e do inquerito
################################################################################

## Inquérito EpiInfo, fase 2
## https://0dea032c-2432-4690-b1e5-636d3cbeb2bf.filesusr.com/ugd/6b3408_08bbcd940e9e4b84a29a7e64fce02464.pdf
## População:  Município de São Paulo, maiores que 17 anos de idade, sem restrição de idade maxima 
## Data de encerramento da fase 2 do inquérito: 24/06/2020
## Prevalência estimada: 11,4% (IC: 9,2% a 13.6%)

## Leitura dos dados mais recentes: sivep residentes
## Executo este script de dentro de scripts_gerenricos/IHR, então uso este path
data.dir <- "../../dados/SIVEP-Gripe/"
## data.dir <- "dados/SIVEP-Gripe/" se rodar do raiz do central_covid
MSPdata <- read.sivep(dir = data.dir, escala = "municipio",sigla = "SP",
                       geocode = 3550308, data = get.last.date(data.dir))
## Calculo do nowcasting de obitos e casos confirmados
lista.MSP <- prepara.sivep(MSPdata, inq.idade = 18, trim.now = 7, window = 40)

##  Projecoes do n de infectados e infectados + resistentes e da prevalência atual
## Populacao maior que 17 anos em 2020
## Usando a populacao estimada em 2020 por faixa etaria, projecoes SEADE
SP.etaria <- read.csv2("../municipio_sp_pop_sp_faixa_etaria_2020.csv")
## Populacao maior que 17 anos
Npop.MSP <- SP.etaria[4,2]*2/5 + sum(SP.etaria[5:16,2])


## Prevalencias projetadas, sem correcao soro-reversão
## Para casos confirmados

## Prevalencia estimada para 01/12/2020 ##
## Estimativa media
## Data para projetar a prevalencia
target.data <- as.Date("2020-12-01")
## Projecoes
MSP.proj.12 <- projeta.inquerito(Npop = Npop.MSP,
                            inq.data = as.Date("2020-06-24"),
                            inq.preval = 0.114,
                            data.proj = target.data,
                            lista = lista.MSP)
## Limite inferior do IC
MSP.proj.12.low <- projeta.inquerito(Npop = Npop.MSP,
                            inq.data = as.Date("2020-06-24"),
                            inq.preval = 0.092,
                            data.proj = target.data,
                            lista = lista.MSP)
## Limite superior do IC
MSP.proj.12.up <- projeta.inquerito(Npop = Npop.MSP,
                            inq.data = as.Date("2020-06-24"),
                            inq.preval = 0.136,
                            data.proj = target.data,
                            lista = lista.MSP)
## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela.MSP.12 <- tab1(MSP.proj.12.low, MSP.proj.12, MSP.proj.12.up)
rownames(tabela.MSP.12)  <- c("IC low", "Média", "IC up")

## Prevalencia estimada para 01/10/2020 ##
## Estimativa media
## Data para projetar a prevalencia
target.data <- as.Date("2020-10-01")
## Projecoes
MSP.proj.10 <- projeta.inquerito(Npop = Npop.MSP,
                            inq.data = as.Date("2020-06-24"),
                            inq.preval = 0.114,
                            data.proj = target.data,
                            lista = lista.MSP)
## Limite inferior do IC
MSP.proj.10.low <- projeta.inquerito(Npop = Npop.MSP,
                            inq.data = as.Date("2020-06-24"),
                            inq.preval = 0.092,
                            data.proj = target.data,
                            lista = lista.MSP)
## Limite superior do IC
MSP.proj.10.up <- projeta.inquerito(Npop = Npop.MSP,
                            inq.data = as.Date("2020-06-24"),
                            inq.preval = 0.136,
                            data.proj = target.data,
                            lista = lista.MSP)
## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela.MSP.10 <- tab1(MSP.proj.10.low, MSP.proj.10, MSP.proj.10.up)
rownames(tabela.MSP.10)  <- c("IC low", "Média", "IC up")


## Prevalencia estimada para 03/01/2021 ##
## Estimativa media
## Data para projetar a prevalencia
target.data <- as.Date("2021-01-03")
## Projecoes
MSP.proj.01 <- projeta.inquerito(Npop = Npop.MSP,
                            inq.data = as.Date("2020-06-24"),
                            inq.preval = 0.114,
                            data.proj = target.data,
                            lista = lista.MSP)
## Limite inferior do IC
MSP.proj.01.low <- projeta.inquerito(Npop = Npop.MSP,
                            inq.data = as.Date("2020-06-24"),
                            inq.preval = 0.092,
                            data.proj = target.data,
                            lista = lista.MSP)
## Limite superior do IC
MSP.proj.01.up <- projeta.inquerito(Npop = Npop.MSP,
                            inq.data = as.Date("2020-06-24"),
                            inq.preval = 0.136,
                            data.proj = target.data,
                            lista = lista.MSP)
## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela.MSP.01 <- tab1(MSP.proj.01.low, MSP.proj.01, MSP.proj.01.up)
rownames(tabela.MSP.01)  <- c("IC low", "Média", "IC up")


## Tabelas para exportar
## Projeções para 01/12/2020
## por IFR
kable(tabela.MSP.12[,c("prevalencia", "IFR", "prev.proj.IFR" )],
      digits = c(1,3,1),
      col.names = c("Prevalência %", "IFR %", "Prevalência projetada"))
## por IHR
kable(tabela.MSP.12[,c("prevalencia", "IHR", "prev.proj.IHR" )],
      digits = c(1,3,1),
      col.names = c("Prevalência %", "IHR %", "Prevalência projetada"))
## Projeções para 01/10/2020
## por IFR
kable(tabela.MSP.10[,c("prevalencia", "IFR", "prev.proj.IFR" )],
      digits = c(1,3,1),
      col.names = c("Prevalência %", "IFR %", "Prevalência projetada"))
## por IHR
kable(tabela.MSP.10[,c("prevalencia", "IHR", "prev.proj.IHR" )],
      digits = c(1,3,1),
      col.names = c("Prevalência %", "IHR %", "Prevalência projetada"))

## Projeções para 03/01/2020
## por IFR
kable(tabela.MSP.01[,c("prevalencia", "IFR", "prev.proj.IFR" )],
      digits = c(1,3,1),
      col.names = c("Prevalência %", "IFR %", "Prevalência projetada"))
## por IHR
kable(tabela.MSP.01[,c("prevalencia", "IHR", "prev.proj.IHR" )],
      digits = c(1,3,1),
      col.names = c("Prevalência %", "IHR %", "Prevalência projetada"))

