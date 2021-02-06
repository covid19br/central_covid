library(ggplot2)
library(readr)
library(knitr)
source("functions.R")
source("../../nowcasting/fct/read.sivep.R")
## Estimativas de total de infectados a partir do dados do inquérito no Maranhão (Silva et al.)
## Maranhão:   a partir de 1 ano de idade, sem restrição de idade maxima 
## 1a fase: 27 July 2020 to 8 August 2020  -  
## Prevalência 40·4% (95%CI 35·6-45·3 ), sem diferenças quanto à idade e sexo
## The infection fatality rate was 0·17%, higher for males and advanced age groups. The ratio of estimated infections to reported cases was 22·2.
## 2a fase: 19 a 30 de outubro de 2020 
## 38,1%, (95%CI 34,8% a 41,4%).

## Leitura dos dados mais recentes: sivep residentes
## Executo este script de dentro de scripts_gerenricos/IHR, então uso este path
data.dir <- "../../dados/SIVEP-Gripe/"
## data.dir <- "dados/SIVEP-Gripe/" se rodar do raiz do central_covid
MAdata <- read.sivep(dir = data.dir, escala = "estado",, sigla = "MA",
                       geocode = 21, data = get.last.date(data.dir))
## Calculo do nowcasting de obitos e casos confirmados, idade minima na amostra do inquérito foi 1 ano
lista.MA <- prepara.sivep(MAdata, inq.idade = 1, trim.now = 7, window = 40)
## Calculo do nowcasting de obitos e casos suspeitos (SRAG), idade minima na amostra do inquérito foi 01 ano
lista.MA.2 <- prepara.sivep(MAdata, inq.idade = 1, trim.now = 7, window = 40, srag =TRUE)

##  Projecoes do n de infectados e infectados + resistentes e da prevalência atual
## Populacao maior que 01 ano em 2020
## Valor obtidoProjecoes populacionais usadas no artigo : https://demografiaufrn.net/laboratorios/lepp/
Npop.MA <- 6997117

## Prevalencias para diferentes momentos, sem correcao soro-reversão
## Para casos confirmados
## Prevalencia de 40.4%, estimada para 08/08/2020
MA.ago <- projeta.inquerito(Npop = Npop.MA, inq.data = as.Date("2020-08-08"), inq.preval = 0.404, lista = lista.MA)
## Prevalencia de 38.1% para 30/10/2020
MA.out <- projeta.inquerito(Npop = Npop.MA, inq.data = as.Date("2020-10-30"), inq.preval = 0.381, lista = lista.MA)

## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela.MA <- tab1(MA.ago, MA.out)

## Usando casos SRAG
## Prevalencia de 40.4%, estimada para 08/08/2020
MA.srag.ago <- projeta.inquerito(Npop = Npop.MA, inq.data = as.Date("2020-08-08"), inq.preval = 0.404, lista = lista.MA.2)
## Prevalencia de 38.1% para 30/10/2020
MA.srag.out <- projeta.inquerito(Npop = Npop.MA, inq.data = as.Date("2020-10-30"), inq.preval = 0.381, lista = lista.MA.2)

## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela.MA.srag <- tab1(MA.srag.ago, MA.srag.out)

## Tabelas para exportar
kable(tabela.MA[,c("data.preval","prevalencia", "IFR", "prev.proj.IFR" )], digits = c(NA,1,3,1))
kable(tabela.MA[,c("data.preval","prevalencia", "IHR", "prev.proj.IHR" )], digits = c(NA,1,3,1))

kable(tabela.MA.srag[,c("data.preval","prevalencia", "IFR", "prev.proj.IFR" )], digits = c(NA,1,3,1))
kable(tabela.MA.srag[,c("data.preval","prevalencia", "IHR", "prev.proj.IHR" )], digits = c(NA,1,3,1))

## Total de casos até  30/10/2020
## Com estimativa de agosto
window(MA.ago$casos.ihr, as.Date("2020-10-30"),as.Date("2020-10-30"))$IR / MA.ago$Npop
window(MA.srag.ago$casos.ihr, as.Date("2020-10-30"),as.Date("2020-10-30"))$IR / MA.srag.ago$Npop


################################################################################
## Graficos
################################################################################
## Grafico de N de novas infecções por dia, estimados pelo IFF e pelo IHR
png("N_novas_infeccoes_calculadas_IHR_mai_2020.png", width = 650)
p1(MA.mai)
dev.off()
png("N_novas_infeccoes_calculadas_IHR_out_2020.png", width = 650)
p1(MA.out)
dev.off()
## Grafico de infectados + resistentes, estimador pelo IFF e IHR
png("Acumuladoinfeccoes_calculadas_IHR_mai_2020.png", width = 650)
p2(MA.mai)
dev.off()

png("Acumuladoinfeccoes_calculadas_IHR_out_2020.png", width = 650)
p2(MA.out)
dev.off()


