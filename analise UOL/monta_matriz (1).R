uol <- read.csv("../SMSSP_covid/analise UOL/SRAGs-tabela-last-updated.csv")
uol2 <- uol[,-2]
uol2[is.na(uol2)] <- 0
## reordena as colunas pela ordem das datas

## matriz para guardar os resultados
uol3 <- matrix( nrow=nrow(uol2), ncol=ncol(uol2)-1)
## Coloca a ultima coluna dos dados na ultima coluna da matriz
uol3[,ncol(uol3)] <- uol2[,ncol(uol2)]
## Loop que vai preenchendo a nova matriz com a difreença da coluna mais a direita pea seguinte
for(i in ncol(uol2):3){
    uol3[,(i-2)] <- uol2[,(i-1)] - uol2[,i]
}
## Conferindo
all(apply(uol3, 1, sum) == uol2[,2]) ## ok!
## Junta com coluna de datas dos boletins e nomeia as colunas
uol3.df <- as.data.frame(uol3)
names(uol3.df) <- names(uol2)[-1]
uol4 <- cbind(data=uol2[,1], uol3.df)

## Work in progress ##
## Monta data.frame de datas de obito e de regitro
datas.boletins <- as.Date( names(uol3.df[,-1]), "X%d.%m.%Y")
datas.obito <- as.Date(as.character(uol4$data), "%d/%m/%Y")
## Vetores para guardar as datas de evento e 
onset.dates <- record.dates <- c()
## Lop sobre as linhas e então colunas da matriz
for(i in 1:length(datas.obito)){
    for(j in 1:length(datas.boletins)){
        if(uol3[i,j]>0){
            onset.dates <- c(onset.dates, rep(datas.obito[i], uol3[i,j]))
            record.dates <- c(record.dates, rep(datas.boletins[j], uol3[i,j]))
        }
    }
}

## Fica com menos mortes que os dados originais, acho que por causa dos valores negativos
length(record.dates)
sum(uol2$X20.04.2020)
