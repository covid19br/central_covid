###Análise UOL###
#################
if(!require(plyr)){install.packages("plyr"); library(plyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(NobBS)){install.packages("NobBS"); library(NobBS)}
if(!require(aweek)){install.packages("aweek"); library(aweek)}
if(!require(cowplot)){install.packages("cowplot"); library(cowplot)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
# if(!require(brms)){install.packages("brms"); library(brms)}
if(!require(rprojroot)){install.packages("rprojroot"); library(rprojroot)}
if(!require(zoo)){install.packages("zoo"); library(zoo)}
if(!require(EpiEstim)){install.packages("EpiEstim"); library(EpiEstim)}
if(!require(foreign)){install.packages("foreign"); library(foreign)}

PRJROOT  = rprojroot::find_root(".here")

source("./funcoes.R")
source("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/_src/funcoes.R")

data_ultimo_boletim<-as.Date("2020-05-08")

uol <- read.csv("./analise UOL/SRAGs-tabela-last-updated_revised.csv")
# uol<-read_csv("./analise UOL/SRAGs-tabela-Nathan.csv")
uol2 <- uol
uol2[is.na(uol2)] <- 0
uol2.1=uol2

for(i in 3:length(uol2)){
  for(k in 1:dim(uol2)[1]){
    if(uol2.1[k,i]>uol2.1[k,(i-1)] & uol2.1[k,i]>0){
      uol2.1[k,i]=uol2.1[k,(i-1)]
    }
  }
}
uol2<-uol2.1
## matriz para guardar os resultados
uol3 <- matrix( nrow=nrow(uol2), ncol=ncol(uol2)-1)
## Coloca a ultima coluna dos dados na ultima coluna da matriz
uol3[,ncol(uol3)] <- uol2[,ncol(uol2)]
## Loop que vai preenchendo a nova matriz com a difreença da coluna mais a direita pra seguinte
for(i in ncol(uol2):3){
  uol3[,(i-2)] <- uol2[,(i-1)] - uol2[,i]
}
## Conferindo
all(apply(uol3, 1, sum) == uol2[,2]) ## ok!
## Junta com coluna de datas dos boletins e nomeia as colunas
uol3.df <- as.data.frame(uol3)
names(uol3.df) <- names(uol2)[-1]
uol4 <- cbind(data=uol2[,1], uol3.df)

## Monta data.frame de datas de obito e de regitro
datas.boletins <- as.character(as.Date( names(uol3.df), "X%d.%m.%y"))
datas.obito <- as.character(as.Date(as.character(uol4$data), "%d/%m/%y"))
## Vetores para guardar as datas de evento e 
onset.dates <- record.dates <- c()
## Loop sobre as linhas e então colunas da matriz
test <- NULL
for(i in length(datas.obito):1){
  for(j in 1:length(datas.boletins)){
    if(uol3[i,j]>0){
      onset.dates  <- c(onset.dates, rep(datas.obito[i], uol3[i,j]))
      record.dates <- c(record.dates, rep(datas.boletins[j], uol3[i,j]))
      test <- c(test, uol3[i,j])
    } else if (uol3[i,j] < 0) {
      N = length(onset.dates) + uol3[i,j]
      onset.dates  <- onset.dates[1:N]
      record.dates <- record.dates[1:N]
    }
  }
}

## Conferindo o total de óbitos ##
length(record.dates)
length(onset.dates)
sum(uol2[,2])

## Monta o data.frame
uol_df <- data.frame(uol_death_date = as.Date(onset.dates), uol_report_date = as.Date(record.dates)) 
## Transformando em datas que entram no nowcasting##
uol_df = uol_df %>%
  mutate(Death_date = as.Date(uol_df$uol_death_date, format = "%d/%m/%Y"), 
         Report_date = as.Date(uol_df$uol_report_date, format = "%d/%m/%Y")) %>%
  as.data.frame()
### somando os óbitos ###
uol_df2 = uol_df %>%
  group_by(Death_date)%>%
  dplyr::summarise(N=n())%>%
  as.data.frame()
