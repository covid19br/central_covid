if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyr)){install.packages("tidyr"); library(tidyr)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(stringr)){install.packages("stringr"); library(stringr)}
source("../nowcasting/fct/get.last.date.R")
source("../nowcasting/fct/read.sivep.R")

#' Função que puxa a distribuição etária para o modelo de Strains das ultimas X semanas ou dias
#'
#' @param estado # Qual estado filtrar para a análise, default Brasil todo 'estado'.
#' @param data_ini # a partir de qual data puxar dados 'data_ini'. Default 16/03/2020.
#' @param data_fim # até qual data puxar dados 'data_fim'. Default 28 dias atrás.
#' @param by_week # Saída com o acumlado por semana 'by_week'. Default FALSE.
#' @param by_age # Saída com dist tempo por idade 'by_age'. Default FALSE.
#' @param more_age_bins # Mais faixas etárias, default de 3 classes: jovens, adultos, idosos. 'more_age_bins'.
#' @param clean_environment # Op??o de deletar dados_brutos ap?s filtrar os dados, Default TRUE.
#' @param overwrite # Carrega novamente os dados da SIVEP mesmo que j? exista no environment. Default FALSE.
#' @param dist_obs # Saída com a distribuição de óbitos 'dist_obs'. Default FALSE.
#'
#' @return # Data.frame com distribuição de casos por faixas etárias por estado por tipo de SRAG por semana.
#' @importFrom magrittr %>%
#'
dist_tempo_estados <- function(estado,
                                data_ini,
                                data_fim,
                                by_week = FALSE,
                                by_age = FALSE,
                                more_age_bins = FALSE,
                                clean_environment = TRUE,
                                overwrite = FALSE,
                                dist_obs = FALSE) {
  
  #1.  Leitura dos dados ####
  ## ISSO É IMPORTANTE PRA READ.SIVEP ##
  data.dir <- "../dados/SIVEP-Gripe/"
  data.sivep <- get.last.date(data.dir)
  
  if (missing(data_fim)){
    data_fim <- Sys.Date() - 28 ## Pega até 4 semanas atrás
  }
  if (missing(data_ini)){
    data_ini <- as.Date("2020-03-16") # Primeira morte registrada?
  }
  
  if (!exists("dados_brutos",mode="list")| overwrite){
    dados_brutos <- read.sivep(dir = data.dir, escala = "pais", data =
                                 data.sivep) %>%
      select(sg_uf, dt_sin_pri, dt_interna, nu_idade_n, hospital, pcr_sars2, classi_fin,
             evolucao)
  }
  
  if (!missing(estado)){
    dados_brutos <- dados_brutos %>% 
      filter(sg_uf == estado)
  }
  
  # Filtrando para data 
  dados <- dados_brutos %>%
    filter(dt_sin_pri <= as_date(data_fim),
           dt_sin_pri >= as_date(data_ini))
  
  # Deixando o environment mais leve
  if(clean_environment) {
    rm(dados_brutos)
    gc()
  }
  
  if (by_age == TRUE){
    ######colocando em classes etárias########
    dados$nu_idade_n <- as.numeric(dados$nu_idade_n)
    if (more_age_bins == TRUE){
      dados <- dados  %>%
        group_by(sg_uf) %>%
        mutate(age_clas = case_when(nu_idade_n >= 0 & nu_idade_n <= 4   ~ 0,
                                    nu_idade_n >= 5 & nu_idade_n <= 9   ~ 5,
                                    nu_idade_n >= 10 & nu_idade_n <= 14 ~ 10,
                                    nu_idade_n >= 15 & nu_idade_n <= 19 ~ 15,
                                    nu_idade_n >= 20 & nu_idade_n <= 24 ~ 20,
                                    nu_idade_n >= 25 & nu_idade_n <= 29 ~ 25,
                                    nu_idade_n >= 30 & nu_idade_n <= 34 ~ 30,
                                    nu_idade_n >= 35 & nu_idade_n <= 39 ~ 35,
                                    nu_idade_n >= 40 & nu_idade_n <= 44 ~ 40,
                                    nu_idade_n >= 45 & nu_idade_n <= 49 ~ 45,
                                    nu_idade_n >= 50 & nu_idade_n <= 54 ~ 50,
                                    nu_idade_n >= 55 & nu_idade_n <= 59 ~ 55,
                                    nu_idade_n >= 60 & nu_idade_n <= 64 ~ 60,
                                    nu_idade_n >= 65 & nu_idade_n <= 69 ~ 65,
                                    nu_idade_n >= 70 & nu_idade_n <= 74 ~ 70,
                                    nu_idade_n >= 75 & nu_idade_n <= 79 ~ 75,
                                    nu_idade_n >= 80 & nu_idade_n <= 84 ~ 80,
                                    nu_idade_n >= 85 & nu_idade_n <= 89 ~ 85,
                                    nu_idade_n >= 90 ~ 90))
    }
    else {
      dados <- dados  %>%
        mutate(age_clas = case_when(nu_idade_n >= 0 & nu_idade_n <= 19 ~ "age_0_19",
                                    nu_idade_n >= 20 & nu_idade_n <= 59 ~ "age_20_59",
                                    nu_idade_n >= 60 ~ "age_60+"))
    }
    #### COVID#########
    covid <- dados %>%
      filter(hospital == 1) %>%
      filter(pcr_sars2 == 1 | classi_fin == 5)  %>%
      filter(!is.na(age_clas) & !is.na(sg_uf)) %>%
      select(dt_sin_pri, dt_interna, age_clas, sg_uf) %>% 
      mutate(tempo_sin_interna = dt_interna - dt_sin_pri) %>% 
      filter(tempo_sin_interna >= 0)
    covid$week <- epiweek(covid$dt_sin_pri)
    
    if(dist_obs == TRUE){
      covid_obs <- dados %>%
        filter(evolucao == 2) %>% 
        filter(pcr_sars2 == 1 | classi_fin == 5)  %>%
        filter(!is.na(age_clas) & !is.na(sg_uf)) %>%
        select(dt_sin_pri, dt_interna, age_clas, sg_uf) %>% 
        mutate(tempo_sin_interna = dt_interna - dt_sin_pri) %>% 
        filter(tempo_sin_interna >= 0)
      covid_obs$week <- epiweek(covid_obs$dt_sin_pri)
    }
    
    
    #### SRAG#########
    srag <- dados %>%
      filter(hospital == 1) %>%
      filter(!is.na(age_clas) & !is.na(sg_uf)) %>%
      select(dt_sin_pri, dt_interna, age_clas, sg_uf) %>% 
      mutate(tempo_sin_interna = dt_interna - dt_sin_pri) %>% 
      filter(tempo_sin_interna >= 0)
    srag$week <- epiweek(srag$dt_sin_pri)
    
    if (dist_obs == TRUE){
      srag_obs<-dados %>% 
        filter(evolucao == 2) %>% 
        filter(!is.na(age_clas) & !is.na(sg_uf)) %>%
        select(dt_sin_pri, dt_interna, age_clas, sg_uf) %>% 
        mutate(tempo_sin_interna = dt_interna - dt_sin_pri) %>% 
        filter(tempo_sin_interna >= 0)
      srag_obs$week <- epiweek(srag_obs$dt_sin_pri)
    }
    
    #### Juntando######
    
    if (by_week == TRUE){
      
      covid_cumsum<-covid %>% 
        group_by(week, sg_uf, age_clas) %>% 
        dplyr::summarise(mean=mean(tempo_sin_interna, na.rm=TRUE), 
                         sd=sd(tempo_sin_interna, na.rm = TRUE))
      
      srag_cumsum<-srag %>% 
        group_by(week, sg_uf, age_clas) %>% 
        dplyr::summarise(mean=mean(tempo_sin_interna, na.rm=TRUE), 
                         sd=sd(tempo_sin_interna, na.rm = TRUE))
      
      dist_cases_hosp<-merge(srag_cumsum, covid_cumsum, 
                             by = c("week", "sg_uf", "age_clas"), 
                             suffixes = c(".srag",".covid"))
      
      if (dist_obs == TRUE){
        covid_obs_cumsum<-covid_obs %>% 
          group_by(week, sg_uf, age_clas) %>% 
          dplyr::summarise(mean=mean(tempo_sin_interna, na.rm=TRUE), 
                           sd=sd(tempo_sin_interna, na.rm = TRUE))
        
        srag_obs_cumsum<-srag_obs %>% 
          group_by(week, sg_uf, age_clas) %>% 
          dplyr::summarise(mean=mean(tempo_sin_interna, na.rm=TRUE), 
                           sd=sd(tempo_sin_interna, na.rm = TRUE))
        
        dist_obs_hosp<-merge(srag_obs_cumsum, covid_obs_cumsum, 
                             by = c("week", "sg_uf", "age_clas"), 
                             suffixes = c(".srag",".covid"))
        
        dist_cases_obs_hosp <- merge(dist_cases_hosp, dist_obs_hosp,
                                     by = c("week", "sg_uf", "age_clas"), 
                                     suffixes = c(".cases", ".deaths")) %>% 
          as.data.frame()
        return(dist_cases_obs_hosp)
      }
      dist_cases_hosp<-dist_cases_hosp %>% 
        as.data.frame()
      return(dist_cases_hosp)
    }
  }
  
  #### COVID####
    covid <- dados %>%
      filter(hospital == 1) %>%
      filter(pcr_sars2 == 1 | classi_fin == 5)  %>%
      filter(!is.na(sg_uf)) %>%
      select(dt_sin_pri, dt_interna, sg_uf) %>% 
      mutate(tempo_sin_interna = dt_interna - dt_sin_pri) %>% 
      filter(tempo_sin_interna >= 0)
    covid$week <- epiweek(covid$dt_sin_pri) 
    
    if(dist_obs == TRUE){
      covid_obs <- dados %>%
        filter(evolucao == 2) %>% 
        filter(pcr_sars2 == 1 | classi_fin == 5)  %>%
        filter(!is.na(sg_uf)) %>%
        select(dt_sin_pri, dt_interna, sg_uf) %>% 
        mutate(tempo_sin_interna = dt_interna - dt_sin_pri) %>% 
        filter(tempo_sin_interna >= 0)
      covid_obs$week <- epiweek(covid_obs$dt_sin_pri)
    }
    
  #### SRAG####
    srag <- dados %>%
      filter(hospital == 1) %>%
      filter(!is.na(sg_uf)) %>%
      select(dt_sin_pri, dt_interna, sg_uf) %>% 
      mutate(tempo_sin_interna = dt_interna - dt_sin_pri) %>% 
      filter(tempo_sin_interna >= 0)
    srag$week <- epiweek(srag$dt_sin_pri)
    
    if (dist_obs == TRUE){
      srag_obs<-dados %>% 
        filter(evolucao == 2) %>% 
        filter(!is.na(sg_uf)) %>%
        select(dt_sin_pri, dt_interna, sg_uf) %>% 
        mutate(tempo_sin_interna = dt_interna - dt_sin_pri) %>% 
        filter(tempo_sin_interna >= 0)
      srag_obs$week <- epiweek(srag_obs$dt_sin_pri)
    }
    
  #### Juntando ######
    
    if (by_week == TRUE){
      
      covid_cumsum<-covid %>% 
        group_by(week, sg_uf) %>% 
        dplyr::summarise(mean_tempo = mean(tempo_sin_interna, na.rm=TRUE), 
                         sd_tempo = sd(tempo_sin_interna, na.rm = TRUE))
      
      srag_cumsum<-srag %>% 
        group_by(week, sg_uf) %>% 
        dplyr::summarise(mean_tempo = mean(tempo_sin_interna, na.rm=TRUE), 
                         sd_tempo = sd(tempo_sin_interna, na.rm = TRUE))
      
      dist_cases_hosp<-merge(srag_cumsum, covid_cumsum, 
                             by = c("week", "sg_uf"), 
                             suffixes = c(".srag",".covid"))
      
      if (dist_obs == TRUE){
        covid_obs_cumsum<-covid_obs %>% 
          group_by(week, sg_uf) %>% 
          dplyr::summarise(mean_tempo = mean(tempo_sin_interna, na.rm=TRUE), 
                           sd_tempo = sd(tempo_sin_interna, na.rm = TRUE))
        
        srag_obs_cumsum<-srag_obs %>% 
          group_by(week, sg_uf) %>% 
          dplyr::summarise(mean_tempo = mean(tempo_sin_interna, na.rm=TRUE), 
                           sd_tempo = sd(tempo_sin_interna, na.rm = TRUE))
        
        dist_obs_hosp<-merge(srag_obs_cumsum, covid_obs_cumsum, 
                             by = c("week", "sg_uf"), 
                             suffixes = c(".srag",".covid"))
        
        dist_cases_obs_hosp <- merge(dist_cases_hosp, dist_obs_hosp,
                                     by = c("week", "sg_uf"), 
                                     suffixes = c(".cases", ".deaths")) %>% 
          as.data.frame()
        return(dist_cases_obs_hosp)
      }
      dist_cases_hosp<-dist_cases_hosp %>% 
        as.data.frame()
      return(dist_cases_hosp)
    }
    
    covid_cumsum<-covid %>% 
      group_by(sg_uf) %>% 
      dplyr::summarise(mean_tempo = mean(tempo_sin_interna, na.rm=TRUE), 
                       sd_tempo = sd(tempo_sin_interna, na.rm = TRUE))
    
    srag_cumsum<-srag %>% 
      group_by(sg_uf) %>% 
      dplyr::summarise(mean_tempo = mean(tempo_sin_interna, na.rm=TRUE), 
                       sd_tempo = sd(tempo_sin_interna, na.rm = TRUE))
    
    dist_cases_hosp<-merge(srag_cumsum, covid_cumsum, 
                           by = c("sg_uf"), 
                           suffixes = c(".srag",".covid"))
    
    if (dist_obs == TRUE){
      covid_obs_cumsum<-covid_obs %>% 
        group_by(sg_uf) %>% 
        dplyr::summarise(mean_tempo = mean(tempo_sin_interna, na.rm=TRUE), 
                         sd_tempo = sd(tempo_sin_interna, na.rm = TRUE))
      
      srag_obs_cumsum<-srag_obs %>% 
        group_by(sg_uf) %>% 
        dplyr::summarise(mean_tempo = mean(tempo_sin_interna, na.rm=TRUE), 
                         sd_tempo = sd(tempo_sin_interna, na.rm = TRUE))
      
      dist_obs_hosp<-merge(srag_obs_cumsum, covid_obs_cumsum, 
                           by = c("sg_uf"), 
                           suffixes = c(".srag",".covid"))
      
      dist_cases_obs_hosp <- merge(dist_cases_hosp, dist_obs_hosp,
                                   by = c("sg_uf"), 
                                   suffixes = c(".cases", ".deaths")) %>% 
        as.data.frame()
      return(dist_cases_obs_hosp)
    }
    dist_cases_hosp<-dist_cases_hosp %>% 
      as.data.frame()
    return(dist_cases_hosp)
}
