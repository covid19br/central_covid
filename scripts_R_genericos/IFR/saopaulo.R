library(ggplot2)
library(readr)
library(readxl)
source("functions.R")
source("../../nowcasting/fct/read.sivep.R")
## Estimativas de total de infectados a partir do dados do inquérito de saopaulo em
## Buss, Lewis F., et al. "Three-quarters attack rate of SARS-CoV-2 in the Brazilian Amazon during a largely unmitigated epidemic." Science 371.6526 (2021): 288-292.
## EM CONSTRUÇÃO, PRECISA PEGAR A PROJEÇÃO DE POPULAÇÃO ACIMA DE 15 ANOS PRA FRENTE

## Leitura dos dados mais recentes: sivep residentes, maiores de 15 anos
data.dir <- "../../dados/SIVEP-Gripe/"
raw.data <- read.sivep(dir = data.dir, escala = "municipio",
                       geocode = 3550308, data = get.last.date(data.dir))
## Calculo do nowcasting de obitos e casos, idade minima na amostra do inquérito foi 16 anos
lista <- prepara.sivep(raw.data, inq.idade = 16, trim.now = 7, window = 40, srag = FALSE)

##  Projecoes do n de infectados e infectados + resistentes e da prevalência atual
## Populacao maior que 15 anos em 2020
## Valor obtidoProjecoes populacionais usadas no artigo : https://demografiaufrn.net/laboratorios/lepp/
## Link nem tão direto para o csv: https://onedrive.live.com/?authkey=!AGYDSnfscBgMkRs&id=CDAF9B19F653F821!57703&cid=CDAF9B19F653F821

ProjMunic_2010_2030 <- read_excel("~/Downloads/ProjMunic-2010_2030.xlsx")
ProjMunic_2010_2030<-ProjMunic_2010_2030 %>% 
  filter(Ano == 2020) %>% 
  as.data.frame()
Npop<-ProjMunic_2010_2030 %>% 
  filter(Armenor == 3550308) %>% 
  as.data.frame()

Npop_15_over<-Npop %>% 
  rowwise() %>% 
  mutate(Over_15 = sum(c(`15 a 20`, `20 a 25`, `30 a 35`, `35 a 40`, `40 a 45`, `45 a 50`, 
                         `50 a 55`, `55 a 60`, `60 a 65`, `65 a 70`, `75 a 80`, `80 a 85`, `85 a 90`, `90+`))) %>% 
  select(Sexo, Over_15)
Npop_15_over<- sum(Npop_15_over[1, "Over_15"] + Npop_15_over[2, "Over_15"])

## Prevalencias para diferentes momentos, com e sem correcao soro-reversão (Table S2)
## https://science.sciencemag.org/content/sci/suppl/2020/12/07/science.abe9728.DC1/abe9728_Buss_SM.pdf
# Mar 9th-21st 0.8 (0.5-1.2)
# Apr 8th-30th 3.1 (2.2-4.4)
# May 8th-21st 6.9 (5.4-9.0)
# Jun 8th-20th 16.1 (14.1-19.4)
# Jul 13th-25th 17.2 (15.6-21.0)
# Aug 10th-29th 22.9 (20.5-28.4)
# Sep 7th-29th 25.0 (22.7-31.6)
# Oct 12th-24th 28.8 (25.9-37.0)
## Prevalencia de 28.8%, estimada para 24/10/2020, com correcao de sororeversão
saopaulo.out <- projeta.inquerito(Npop = Npop_15_over, inq.data = as.Date("2020-10-17"), inq.preval = 0.288, lista = lista)
## Prevalencia de 25% 29/09/2020, com correcao de sororeversao
saopaulo.set <- projeta.inquerito(Npop = Npop_15_over, inq.data = as.Date("2020-09-14"), inq.preval = 0.25, lista = lista)
## Prevalencia de 22.9% 29/08/2020, com correcao de sororeversao
saopaulo.ago <- projeta.inquerito(Npop = Npop_15_over, inq.data = as.Date("2020-08-19"), inq.preval = 0.229, lista = lista)
## Prevalencia de 17.2% 25/07/2020, com correcao de sororeversao
saopaulo.jul <- projeta.inquerito(Npop = Npop_15_over, inq.data = as.Date("2020-07-15"), inq.preval = 0.172, lista = lista)
## Prevalencia de 16.1% 20/06/2020, com correcao de sororeversao
saopaulo.jun <- projeta.inquerito(Npop = Npop_15_over, inq.data = as.Date("2020-06-15"), inq.preval = 0.161, lista = lista)
## Prevalencia de 6.9% 21/05/2020, com correcao de sororeversao (que neste momento é mínima)
saopaulo.mai <- projeta.inquerito(Npop = Npop_15_over, inq.data = as.Date("2020-05-14"), inq.preval = 0.069, lista = lista)
## Prevalencia de 3.1% em  30/04/2020, com correcao de sororeversao (que neste momento é mínima)
saopaulo.abr <- projeta.inquerito(Npop = Npop_15_over, inq.data = as.Date("2020-04-17"), inq.preval = 0.031, lista = lista)
## Prevalencia de 0.8% em  12/03/2020, com correcao de sororeversao (que neste momento é mínima)
saopaulo.mar <- projeta.inquerito(Npop = Npop_15_over, inq.data = as.Date("2020-03-12"), inq.preval = 0.008, lista = lista)

## Uma tabela com os valores de prevalência atuais projetados com cada uma das prevalencias acima
tabela <- tab1(saopaulo.abr, saopaulo.mai, saopaulo.jun, saopaulo.jul, saopaulo.ago, saopaulo.set, saopaulo.out)

## Tabelas para exportar
kable(tabela[,c("data.preval","prevalencia", "IFR", "prev.proj.IFR" )], digits = c(NA,1,3,1))
kable(tabela[,c("data.preval","prevalencia", "IHR", "prev.proj.IHR" )], digits = c(NA,1,3,1))


################################################################################
## Graficos
################################################################################
## Grafico de N de novas infecções por dia, estimados pelo IFF e pelo IHR
png("N_novas_infeccoes_calculadas_IHR_mai_2020.png", width = 650)
p1(saopaulo.mai)
dev.off()
png("N_novas_infeccoes_calculadas_IHR_out_2020.png", width = 650)
p1(saopaulo.out)
dev.off()
## Grafico de infectados + resistentes, estimador pelo IFF e IHR
png("Acumuladoinfeccoes_calculadas_IHR_mai_2020.png", width = 650)
p2(saopaulo.mai)
dev.off()

png("Acumuladoinfeccoes_calculadas_IHR_out_2020.png", width = 650)
p2(saopaulo.out)
dev.off()