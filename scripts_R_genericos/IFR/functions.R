library(dplyr)
library(plyr)
library(tidyr)
library(zoo)
library(lubridate)
library(NobBS)
library(aweek)
library(ggplot2)
source("../../nowcasting/fct/gera.nowcasting.R")
source("../../nowcasting/fct/write.notificacoes.data.R")
source("../../nowcasting/fct/prepara_dados2.R")
source("../../nowcasting/fct/get.last.date.R")
source("../../nowcasting/fct/preenche.now.R")


#' @param sivep sivep importada com a função read.sivep
#' @param inq.idade idade mínimo de inclusão no inquérito
#' @param srag lógico usar casos de srag ou apenas os confirmados de
#'     covid
#' @param ... outros argumentos para a função gera.nowcasting que fará
#'     os nowcastings. Importante especificar trim.now ewindow
prepara.sivep <- function(sivep, inq.idade, srag = FALSE, ...){
    if(srag)
        tipo  <- "srag"
    else
        tipo <- "covid"
    ## Filtra apenas casos com a idade minima do inquérito
    ## Que é o universo amostral do inquérito
    dados <- filter(sivep, as.integer(nu_idade_n) >= inq.idade)
    ## Nowcasting de obitos, com data de sintoma como onset
    ob.now <-  gera.nowcasting(dados,
                               caso = FALSE,
                               tipo = tipo,
                               hospitalizados = FALSE,
                               obito_sin_pri = TRUE, ...) 
    ##Nowcasting de casos hospitalizados
    casos.now <-  gera.nowcasting(dados,
                                  caso = TRUE,
                                  tipo = tipo,
                                  hospitalizados = TRUE, ...)
    ## N de casos por datas de notificacao e primeiro sintoma
    obitos <- write.notificacoes.data(ob.now$dados, tipo = paste0("obitos_", tipo,"_data_sin"), write.arq = FALSE)
    casos <- write.notificacoes.data(casos.now$dados, tipo = tipo, write.arq = FALSE)
    ## junta nowcasting com n de casos por data de sintoma
    obitos.merge <- prepara_dados2(ob.now, obitos)
    casos.merge <- prepara_dados2(casos.now, casos)
    return(list(dados = dados, casos = casos.merge, obitos=obitos.merge))
}

#' @param PopTot tamanho da população total amostrada.
#' @param inq.data data final do inquérito
#' @param inq.preval prevalência estimada pelo inquérito
#' @param data.proj data para a qual projetar a pervalência. Se não
#'     fornecida utiliza-se a data mais recente no objeto indicado
#'     pleo argumento lista, abaixo.
#' @param lista lista com os dados e nowcasting, retornada pela função
#'     prepara.sivep
projeta.inquerito <- function(Npop, inq.data, inq.preval, data.proj, lista) {
    ## uso como referencia a data do fim da amostra menos 7 dias (tempo de resposta imune)
    ref.data <- inq.data - 7
    ## Total de infectados e recuperados no inquerito
    inq.IR <- Npop * inq.preval    
    ## Calculo do IFR
    ## Separando data frame para facilitar
    ob.ifr <- lista$obitos$now.pred.zoo
    ## IFR
    ref.data2 <- max(time(ob.ifr)[time(ob.ifr)<=ref.data])
    IFR <- as.numeric(ob.ifr$estimate.merged.c[time(ob.ifr)==ref.data2] / inq.IR)
    ## calculo do n de pessoas no compartimento IR e I a cada tempo
    ob.ifr$IR  <- ob.ifr$estimate.merged.c / IFR
    ob.ifr$I <- c(ob.ifr$IR[1], diff(ob.ifr$IR))
    ## Calculo do IHR e do n de novas infeccoes por dia
    ## Separando data frame para facilitar
    casos.ihr <- lista$casos$now.pred.zoo
    ## IHR
    ## data de referencia
    ref.data2 <- max(time(casos.ihr)[time(casos.ihr)<=ref.data])
    IHR <- as.numeric(casos.ihr$estimate.merged.c[time(casos.ihr)==ref.data2] / inq.IR)
    ## calculo do n de pessoas no compartimento IR e I a cada tempo
    casos.ihr$IR  <- casos.ihr$estimate.merged.c / IHR
    casos.ihr$I <- c(casos.ihr$IR[1], diff(casos.ihr$IR))
    ##casos.ihr <- merge.zoo(casos.ihr, n.not= zoo(lista$n.not[,2],lista$n.not[,1])) ## Removido por enquanto, era pra junta n de casos notificados em cada data, mas a prepara.dados2 nao está guardando n de casos por data de notificacao
    ## Prevalencias nas datas mais recentes na sivep(veja tb os graficos, abaixo)
    ## Usando IFR
    if(missing(data.proj))
        criterio <- time(ob.ifr)[ob.ifr$IR==max(ob.ifr$IR, na.rm=TRUE)]
    else
        criterio <- max(time(ob.ifr)[time(ob.ifr)<=data.proj])
    prev.atual.ifr <- ob.ifr$IR[time(ob.ifr)==criterio] / Npop
    cat("\n Prevalência com IFR projetada para ", format(criterio, "%d de %B de %Y"), "\n")
    ## Usando o IHR
    if(missing(data.proj))
        criterio <- time(casos.ihr)[casos.ihr$IR==max(casos.ihr$IR, na.rm=TRUE)]
    else
        criterio <- max(time(casos.ihr)[time(casos.ihr)<=data.proj])
    prev.atual.ihr <- casos.ihr$IR[time(casos.ihr) == criterio] / Npop
    cat("\n Prevalência com IHR projetada para ", format(criterio, "%d de %B de %Y"), "\n")
    ## Guarda todos os resultados em uma lista
    list(lista.dados = lista,
         Npop=Npop,
         inq.data=inq.data,
         ref.data = ref.data,
         inq.IR=inq.IR,
         inq.preval = inq.preval,
         ob.ifr = ob.ifr,
         casos.ihr = casos.ihr,
         IFR = IFR, 
         IHR = IHR,
         prev.atual.ihr = prev.atual.ihr,
         prev.atual.ifr = prev.atual.ifr)
 }

## Graficos
## Grafico de N de novas infecções por dia, estimados pelo IFF e pelo IHR
p1 <- function(lista){
    lista$ob.ifr %>%
        fortify() %>%
        ggplot(aes(Index, I)) +
        geom_line(aes(color="IFR")) +
        geom_line(data = fortify(lista$casos.ihr), aes(Index, I, color="IHR")) +
        ## geom_line(data = lista$casos.ihr, aes(Index, n.not, color="Notificados")) +
        ylab("Novas infecções")
}
## Grafico de infectados + resistentes, estimador pelo IFF e IHR
p2 <- function(lista){
    lista$ob.ifr %>%
        fortify() %>%
        ggplot(aes(Index, IR)) +
        geom_line(aes(color="IFR")) +
        geom_line(data = fortify(lista$casos.ihr), aes(Index, IR, color="IHR")) +
        geom_abline(intercept = lista$Npop, col = "black") +
        ylab("Infectados + Resistentes") +
        theme_bw()
}

## tabela com estimativas e projecoes de prevalencia
tab1 <- function(..., nomes = FALSE){
    lista <- list(...)
    if(nomes)
        names(lista)  <- as.list(match.call())[-1]
    f1 <- function(x){
        with(x,
             data.frame(
                 data.preval =inq.data,
                 prevalencia =inq.preval*100,
                 IFR = IFR*100,
                 IHR = IHR*100,
                 data.proj.IHR= max(time(casos.ihr)),
                 prev.proj.IHR = prev.atual.ihr*100,
                 data.proj.IFR =max(time(ob.ifr)),
                 prev.proj.IFR =prev.atual.ifr*100))
    }
    ldply(lista, f1 )
    
}
