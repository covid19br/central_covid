library(ggplot2)
library(readr)
library(knitr)
source("functions.R")
source("../../nowcasting/fct/read.sivep.R")
## Estimativas de total de infectados a partir do dados do inquérito de Manaus em
## Buss, Lewis F., et al. "Three-quarters attack rate of SARS-CoV-2 in the Brazilian Amazon during a largely unmitigated epidemic." Science 371.6526 (2021): 288-292.


## Leitura dos dados mais recentes: sivep residentes, maiores de 15 anos
## Executo este script de dentro de scripts_gerenricos/IHR, então uso este path
data.dir <- "../../dados/SIVEP-Gripe/"
## data.dir <- "dados/SIVEP-Gripe/" se rodar do raiz do central_covid
raw.data <- read.sivep(dir = data.dir, escala = "municipio",
                       geocode = 1302603, data = get.last.date(data.dir))
## Calculo do nowcasting de obitos e casos confirmados, idade minima na amostra do inquérito foi 16 anos
lista <- prepara.sivep(raw.data, inq.idade = 16, trim.now = 7, window = 40)
## Calculo do nowcasting de obitos e casos suspeitos (SRAG), idade minima na amostra do inquérito foi 16 anos
lista2 <- prepara.sivep(raw.data, inq.idade = 16, trim.now = 7, window = 40, srag =TRUE)

##  Projecoes do n de infectados e infectados + resistentes e da prevalência atual
## Populacao maior que 15 anos em 2020
## Valor obtidoProjecoes populacionais usadas no artigo : https://demografiaufrn.net/laboratorios/lepp/
Npop <- 1675488

## Prevalencias para diferentes momentos, com e sem correcao soro-reversão (Table S2)
## https://science.sciencemag.org/content/sci/suppl/2020/12/07/science.abe9728.DC1/abe9728_Buss_SM.pdf
## Para casos confirmados
## Prevalencia de 76%, estimada para 17/10/2020, com correcao de sororeversão
manaus.out <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-10-17"), inq.preval = 0.76, lista = lista)
## Prevalencia de 72,2% 14/09/2020, com correcao de sororeversao
manaus.set <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-09-14"), inq.preval = 0.722, lista = lista)
## Prevalencia de 66,2% 19/08/2020, com correcao de sororeversao
manaus.ago <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-08-19"), inq.preval = 0.662, lista = lista)
## Prevalencia de 66,2% 15/07/2020, com correcao de sororeversao
manaus.jul <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-07-15"), inq.preval = 0.662, lista = lista)
## Prevalencia de 65,2% 15/06/2020, com correcao de sororeversao
manaus.jun <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-06-15"), inq.preval = 0.652, lista = lista)
## Prevalencia de 44,5% 14/05/2020, com correcao de sororeversao (que neste momento é mínima)
manaus.mai <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-05-14"), inq.preval = 0.459, lista = lista)
## Prevalencia de 4,8% em  17/04/2020, com correcao de sororeversao (que neste momento é mínima)
manaus.abr <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-04-17"), inq.preval = 0.05, lista = lista)
## Prevalencia de 0,7% em  12/03/2020, com correcao de sororeversao (que neste momento é mínima), nao funfa
##manaus.mar <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-03-12"), inq.preval = 0.007, lista = lista)

## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela <- tab1(manaus.abr, manaus.mai, manaus.jun, manaus.jul, manaus.ago, manaus.set, manaus.out)

## Para casos suspeitos (SRAG)
## Prevalencia de 76%, estimada para 17/10/2020, com correcao de sororeversão
manaus.srag.out <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-10-17"), inq.preval = 0.76, lista = lista2)
## Prevalencia de 72,2% 14/09/2020, com correcao de sororeversao
manaus.srag.set <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-09-14"), inq.preval = 0.722, lista = lista2)
## Prevalencia de 66,2% 19/08/2020, com correcao de sororeversao
manaus.srag.ago <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-08-19"), inq.preval = 0.662, lista = lista2)
## Prevalencia de 66,2% 15/07/2020, com correcao de sororeversao
manaus.srag.jul <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-07-15"), inq.preval = 0.662, lista = lista2)
## Prevalencia de 65,2% 15/06/2020, com correcao de sororeversao
manaus.srag.jun <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-06-15"), inq.preval = 0.652, lista = lista2)
## Prevalencia de 44,5% 14/05/2020, com correcao de sororeversao (que neste momento é mínima)
manaus.srag.mai <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-05-14"), inq.preval = 0.459, lista = lista2)
## Prevalencia de 4,8% em  17/04/2020, com correcao de sororeversao (que neste momento é mínima)
manaus.srag.abr <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-04-17"), inq.preval = 0.05, lista = lista2)
## Prevalencia de 0,7% em  12/03/2020, com correcao de sororeversao (que neste momento é mínima) não funfa
##manaus.srag.mar <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-03-12"), inq.preval = 0.007, lista = lista)

## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela.srag <- tab1(manaus.srag.abr, manaus.srag.mai, manaus.srag.jun, manaus.srag.jul, manaus.srag.ago, manaus.srag.set, manaus.srag.out)


## Tabelas para exportar
kable(tabela[,c("data.preval","prevalencia", "IFR", "prev.proj.IFR" )], digits = c(NA,1,3,1))
kable(tabela[,c("data.preval","prevalencia", "IHR", "prev.proj.IHR" )], digits = c(NA,1,3,1))

kable(tabela.srag[,c("data.preval","prevalencia", "IFR", "prev.proj.IFR" )], digits = c(NA,1,3,1))
kable(tabela.srag[,c("data.preval","prevalencia", "IHR", "prev.proj.IHR" )], digits = c(NA,1,3,1))

## Total de casos até  01/12/2020
## Com estimativa de outubro
window(manaus.out$casos.ihr, as.Date("2020-12-01"),as.Date("2020-12-01"))$IR / manaus.out$Npop
window(manaus.srag.out$casos.ihr, as.Date("2020-12-01"),as.Date("2020-12-01"))$IR / manaus.srag.out$Npop
## Com estimativa de junho
window(manaus.jun$casos.ihr, as.Date("2020-12-01"),as.Date("2020-12-01"))$IR / manaus.jun$Npop
window(manaus.srag.jun$casos.ihr, as.Date("2020-12-01"),as.Date("2020-12-01"))$IR / manaus.srag.jun$Npop 


## Preprint de Lalwani et al ##
##https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3774177
## "Using these data, we estimated a 27% antibody decay for the whole cohort, putting a maximum disease prevalence at 41.53%."
## Não esta claro quando terminou a coleta, pela figura S3 parece ser fim de outubro
## Prevalencia de 41,53%, estimada para 30/09/2020
## Calculo do nowcasting de obitos e casos confirmados, idade minima na amostra do inquérito foi 18 anos
lista3 <- prepara.sivep(raw.data, inq.idade = 18, trim.now = 7, window = 40)
## Calculo do nowcasting de obitos e casos suspeitos (SRAG), idade minima na amostra do inquérito foi 18 anos
lista4 <- prepara.sivep(raw.data, inq.idade = 18, trim.now = 7, window = 40, srag =TRUE)

##  Projecoes do n de infectados e infectados + resistentes e da prevalência atual
## Populacao maior que 17 anos em 2020
## Valor obtidoProjecoes populacionais usadas no artigo : https://demografiaufrn.net/laboratorios/lepp/
Npop2 <- 1563172


## COVID
manaus.lalwani <- projeta.inquerito(Npop = Npop2, inq.data = as.Date("2020-10-02"), inq.preval = 0.4153, lista = lista3)
## SRAG
manaus.srag.lalwani <- projeta.inquerito(Npop = Npop2, inq.data = as.Date("2020-10-02"), inq.preval = 0.4153, lista = lista4)
## Tabela
tab.lalwani <- tab1(manaus.lalwani, manaus.srag.lalwani)
## Tabela: casos confirmados e SRAG
kable(tab.lalwani[,c("data.preval","prevalencia", "IFR", "prev.proj.IFR" )], digits = c(NA,1,3,1))
kable(tab.lalwani[,c("data.preval","prevalencia", "IHR", "prev.proj.IHR" )], digits = c(NA,1,3,1))
## Prevalencias em 01/12/2020
window(manaus.lalwani$casos.ihr, as.Date("2020-12-01"),as.Date("2020-12-01"))$IR / manaus.lalwani$Npop
window(manaus.srag.lalwani$casos.ihr, as.Date("2020-12-01"),as.Date("2020-12-01"))$IR / manaus.srag.lalwani$Npop 

################################################################################
## Graficos
################################################################################
## Grafico de N de novas infecções por dia, estimados pelo IFF e pelo IHR
png("N_novas_infeccoes_calculadas_IHR_mai_2020.png", width = 650)
p1(manaus.mai)
dev.off()
png("N_novas_infeccoes_calculadas_IHR_out_2020.png", width = 650)
p1(manaus.out)
dev.off()
## Grafico de infectados + resistentes, estimador pelo IFF e IHR
png("Acumuladoinfeccoes_calculadas_IHR_mai_2020.png", width = 650)
p2(manaus.mai)
dev.off()

png("Acumuladoinfeccoes_calculadas_IHR_out_2020.png", width = 650)
p2(manaus.out)
dev.off()


