## Funções ad para tratar arquivos e fazer graficos para o relatório de comparação
## Super ad hoc: quebram com facilidade


## Filtra dados dos estados da planilha infogripe e prepara para os plots
info.estado <- function(x){
    filter(x, Tipo == "Estado"&
              `Ano epidemiológico` > 2019 &
              ## data.de.publicação == max(data.de.publicação) &
              dado == "srag" &
              escala == "casos") %>%
        select(UF,
               `Unidade da Federação`,
               `Ano epidemiológico`,
               `Semana epidemiológica`,
               `Casos semanais reportados até a última atualização`,
               `limite inferior da estimativa`,
               `casos estimados`,
               `limite superior da estimativa`) %>%
        merge(dic.estados[, 2:3], by.x = "UF", by.y = "id") %>%
        dplyr::rename(id.estado = UF,
                      nome.estado = `Unidade da Federação`,
                      semana = `Semana epidemiológica`,
                      casos.obs = `Casos semanais reportados até a última atualização`,
                      lower = `limite inferior da estimativa`,
                      casos.est = `casos estimados`,
                      upper = `limite superior da estimativa`,
                      sigla.estado = sigla) %>%
        mutate(semana = ifelse(`Ano epidemiológico` == 2021, semana + 53, semana),
               semana = ifelse(`Ano epidemiológico` == 2022, semana + 52, semana))
}

## Transforma o par ano, semana dos dados Infogripe no formato de epiweek
epi.data <- function(semana, ano){
    sem.f <- as.character(semana)
    sem.f <- ifelse(semana <10, paste0("0",sem.f), sem.f)
    paste0(ano,"-W",sem.f,"-",1)
    }

################################################################################
## Funcoes para graficos
################################################################################


## Funcao para graficos semanais de SRAG: todos os filtros do Infogripe e o nowcasting Observatório
plot.seman <- function(sigla){
    ##max.sem <- observ.data.max$sem.max[observ.data.max$estado==sigla]
    data1  <-  infogr.estado %>% filter(sigla.estado == sigla) 
    ##data2 <- observ.seman %>% filter(semana <= (max.sem) & sigla.estado == sigla)
    data2 <- observ.seman %>% filter(estado == sigla)
    data3  <-  infogr2.estado %>% filter(sigla.estado == sigla)
    data4  <-  infogr3.estado %>% filter(sigla.estado == sigla) 
    p1 <- ggplot(data1, aes(semana)) +
        geom_line(aes(y=casos.obs, color = "InfoGripe sem filtros"), lty =2 ) +
        geom_line(aes(y=casos.est, color = "InfoGripe sem filtros")) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill= "InfoGripe sem filtros"), alpha =0.1) +
        geom_line(data = data3, aes(y=casos.obs, color = "InfoGripe com filtros"), lty =2) +
        geom_line(data = data3, aes(y=casos.est, color = "InfoGripe com filtros")) +
        geom_ribbon(data=data3, aes(ymin = lower, ymax = upper, fill = "InfoGripe com filtros"), alpha =0.1) +
        geom_line(data = data4, aes(y=casos.obs, color = "InfoGripe filtro febre"), lty =2) +
        geom_line(data = data4, aes(y=casos.est, color = "InfoGripe filtro febre")) +
        geom_ribbon(data = data4, aes(ymin = lower, ymax = upper, fill = "InfoGripe filtro febre"), alpha =0.1) +
        geom_line(data = data2, aes(y=n.casos, color= "Observatório"), lty =2 ) +
        geom_line(data = data2, aes(y=estimate, color= "Observatório")) +
        geom_ribbon(data = data2, aes(ymin = lower.merged.pred, ymax = upper.merged.pred, fill= "Observatório"),
                    alpha =0.1) +
        xlab("Semana epidemiológica dos sintomas") +
            ylab("N de casos semanais SRAG") +
        scale_color_manual(name="", values = 5:2) +
        scale_fill_manual(name = "", values = 5:2) +
        ggtitle(sigla) +
        theme_bw() +
        theme(legend.position = c(0.15, 0.8))
    p1
}

## SRAG sem filtros Infogripe, nowcasting por dif de bases e Observatório
plot.seman2 <- function(sigla){
    data1  <-  infogr.estado %>% filter(sigla.estado == sigla) 
    data2 <- observ.seman %>% filter(estado == sigla)
    data3  <-  difsragh %>% filter(sigla.estado == sigla)
    p1 <-
        ggplot(data1, aes(semana)) +
        geom_line(aes(y=casos.obs, color = "InfoGripe sem filtros"), lty =2 ) +
        geom_line(aes(y=casos.est, color = "InfoGripe sem filtros")) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill= "InfoGripe sem filtros"), alpha =0.2) +
        geom_line(data = data3, aes(y=casos.obs, color = "Dif. bases"), lty =2) +
        geom_line(data = data3, aes(y=casos.est, color = "Dif. bases")) +
        geom_ribbon(data=data3, aes(ymin = lower, ymax = upper, fill = "Dif. bases"), alpha =0.2) +
        geom_line(data = data2, aes(y=n.casos, color= "Observatório"), lty =2 ) +
        geom_line(data = data2, aes(y=estimate, color= "Observatório")) +
        geom_ribbon(data = data2, aes(ymin = lower.merged.pred, ymax = upper.merged.pred, fill= "Observatório"),
                    alpha =0.2) +
        xlab("Semana epidemiológica dos sintomas") +
            ylab("N de casos semanais SRAG") +
        scale_color_manual(name="", values = 4:2) +
        scale_fill_manual(name = "", values = 4:2) +
        ggtitle(sigla) +
        theme_bw() +
        theme(legend.position = c(0.15, 0.8))
    p1
}
