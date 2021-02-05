library(ggplot2)
library(readr)
source("functions.R")
source("../../nowcasting/fct/read.sivep.R")
## Estimativas de total de infectados a partir do dados do inquérito de Manaus em
## Buss, Lewis F., et al. "Three-quarters attack rate of SARS-CoV-2 in the Brazilian Amazon during a largely unmitigated epidemic." Science 371.6526 (2021): 288-292.


## Leitura dos dados mais recentes: sivep residentes, maiores de 15 anos
data.dir <- "dados/SIVEP-Gripe/"
raw.data <- read.sivep(dir = data.dir, escala = "municipio",
                       geocode = 3550308, data = get.last.date(data.dir))
## Calculo do nowcasting de obitos e casos, idade minima na amostra do inquérito foi 16 anos
lista <- prepara.sivep(raw.data, inq.idade = 16, trim.now = 7, window = 40)

##  Projecoes do n de infectados e infectados + resistentes e da prevalência atual
## Populacao maior que 15 anos em 2020
## Valor obtidoProjecoes populacionais usadas no artigo : https://demografiaufrn.net/laboratorios/lepp/
Npop <- 1675488

## Prevalencias para diferentes momentos, com e sem correcao soro-reversão (Table S2)
## https://science.sciencemag.org/content/sci/suppl/2020/12/07/science.abe9728.DC1/abe9728_Buss_SM.pdf
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
## Prevalencia de 0,7% em  12/03/2020, com correcao de sororeversao (que neste momento é mínima)
manaus.mar <- projeta.inquerito(Npop = Npop, inq.data = as.Date("2020-03-12"), inq.preval = 0.007, lista = lista)

## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela <- tab1(manaus.abr, manaus.mai, manaus.jun, manaus.jul, manaus.ago, manaus.set, manaus.out)

## Tabelas para exportar
kable(tabela[,c("data.preval","prevalencia", "IFR", "prev.proj.IFR" )], digits = c(NA,1,3,1))
kable(tabela[,c("data.preval","prevalencia", "IHR", "prev.proj.IHR" )], digits = c(NA,1,3,1))


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


