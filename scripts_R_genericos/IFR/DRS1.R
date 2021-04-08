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
DRS1data <- read.sivep(dir = data.dir, escala = "drs", sigla = "SP",
                       geocode = 1, data = get.last.date(data.dir))
## Calculo do nowcasting de obitos e casos confirmados
lista.DRS1 <- prepara.sivep(DRS1data, inq.idade = 18, trim.now = 7, window = 40)
## Populacao maior que 17 anos em 2020
## Usando a populacao estimada em 2020 por faixa etaria, projecoes SEADE
## https://produtos.seade.gov.br/produtos/projpop/index.php
Npop.DRS1 <- 591536 + 15593029 ## faixa 18-19 + faixa 20 anos ou mais, segundo a projecao no site

## Prevalencias projetadas, a partir das IHR e IFR estmados para o municipio de SP,
## com a fase 2 do inquerito do SoroEpi
## Para casos confirmados
## Leitura dos valores de IHR e IFR
DRS1.IHFR <- read.csv("IHR_IFR_casos_confirmados_SoroEpi_fase2.csv", row.names = 1)

## Prevalencia estimada para 01/10/2020 ##
## Data para projetar a prevalencia
target.data <- as.Date("2020-10-01")
## Projecoes
DRS1.proj.10 <- projeta.IHR(Npop = Npop.DRS1,
                            IHR = DRS1.IHFR["Média","IHR"],
                            IFR = DRS1.IHFR["Média","IFR"],
                            data.proj = target.data,
                            lista = lista.DRS1)
## Limite inferior do IC
## Projecoes
DRS1.proj.10.low <- projeta.IHR(Npop = Npop.DRS1,
                            IHR = DRS1.IHFR["IC low","IHR"],
                            IFR = DRS1.IHFR["IC low","IFR"],
                            data.proj = target.data,
                            lista = lista.DRS1)
## Limite superior do IC
DRS1.proj.10.up <- projeta.IHR(Npop = Npop.DRS1,
                            IHR = DRS1.IHFR["IC up","IHR"],
                            IFR = DRS1.IHFR["IC up","IFR"],
                            data.proj = target.data,
                            lista = lista.DRS1)
## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela.DRS1.10 <- tab2(DRS1.proj.10.low, DRS1.proj.10, DRS1.proj.10.up)
rownames(tabela.DRS1.10)  <- c("IC low", "Média", "IC up")

## Prevalencia estimada para 01/12/2020 ##
## Estimativa media
## Data para projetar a prevalencia
target.data <- as.Date("2020-12-01")
## Projecoes
DRS1.proj.12 <- projeta.IHR(Npop = Npop.DRS1,
                            IHR = DRS1.IHFR["Média","IHR"],
                            IFR = DRS1.IHFR["Média","IFR"],
                            data.proj = target.data,
                            lista = lista.DRS1)
## Limite inferior do IC
## Projecoes
DRS1.proj.12.low <- projeta.IHR(Npop = Npop.DRS1,
                            IHR = DRS1.IHFR["IC low","IHR"],
                            IFR = DRS1.IHFR["IC low","IFR"],
                            data.proj = target.data,
                            lista = lista.DRS1)
## Limite superior do IC
DRS1.proj.12.up <- projeta.IHR(Npop = Npop.DRS1,
                            IHR = DRS1.IHFR["IC up","IHR"],
                            IFR = DRS1.IHFR["IC up","IFR"],
                            data.proj = target.data,
                            lista = lista.DRS1)
## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela.DRS1.12 <- tab2(DRS1.proj.12.low, DRS1.proj.12, DRS1.proj.12.up)
rownames(tabela.DRS1.12)  <- c("IC low", "Média", "IC up")


## Prevalencia estimada para 03/01/2021 ##
## Estimativa media
## Data para projetar a prevalencia
target.data <- as.Date("2021-01-03")
## Projecoes
DRS1.proj.01 <- projeta.IHR(Npop = Npop.DRS1,
                            IHR = DRS1.IHFR["Média","IHR"],
                            IFR = DRS1.IHFR["Média","IFR"],
                            data.proj = target.data,
                            lista = lista.DRS1)
## Limite inferior do IC
## Projecoes
DRS1.proj.01.low <- projeta.IHR(Npop = Npop.DRS1,
                            IHR = DRS1.IHFR["IC low","IHR"],
                            IFR = DRS1.IHFR["IC low","IFR"],
                            data.proj = target.data,
                            lista = lista.DRS1)
## Limite superior do IC
DRS1.proj.01.up <- projeta.IHR(Npop = Npop.DRS1,
                            IHR = DRS1.IHFR["IC up","IHR"],
                            IFR = DRS1.IHFR["IC up","IFR"],
                            data.proj = target.data,
                            lista = lista.DRS1)
## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela.DRS1.01 <- tab2(DRS1.proj.01.low, DRS1.proj.01, DRS1.proj.01.up)
rownames(tabela.DRS1.01)  <- c("IC low", "Média", "IC up")

## Tabelas para exportar
## Projeções para 01/10/2020
## por IFR
sink("DRS1_SoroEpi.out")
cat("\n Projeções para 01/10/2020 \n")
kable(tabela.DRS1.10[,c("IFR", "prev.proj.IFR" )],
      digits = c(2,1),
      col.names = c("IFR %", "Prevalência projetada"))
## por IHR
kable(tabela.DRS1.10[,c("IHR", "prev.proj.IHR" )],
      digits = c(2,1),
      col.names = c("IHR %", "Prevalência projetada"))

## Projeções para 01/12/2020
## por IFR
cat("\n Projeções para 01/12/2020 \n")
kable(tabela.DRS1.12[,c("IFR", "prev.proj.IFR" )],
      digits = c(2,1),
      col.names = c("IFR %", "Prevalência projetada"))
## por IHR
kable(tabela.DRS1.12[,c("IHR", "prev.proj.IHR" )],
      digits = c(2,1),
      col.names = c("IHR %", "Prevalência projetada"))

## Projeções para 03/01/2020
## por IFR
cat("\n Projeções para 03/01/2021 \n")
kable(tabela.DRS1.01[,c("IFR", "prev.proj.IFR" )],
      digits = c(2,1),
      col.names = c("IFR %", "Prevalência projetada"))
## por IHR
kable(tabela.DRS1.01[,c("IHR", "prev.proj.IHR" )],
      digits = c(2,1),
      col.names = c("IHR %", "Prevalência projetada"))
sink()
