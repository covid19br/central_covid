format_betas_output <- function(dir = "./nowcasting/Estados_betas/estado_",
                                escala = "estado",
                                sigla = "SP",
                                analise = c("betas_obitos_covid_median")){
  arquivos_output <-
    list.files(
      path = paste0(dir, sigla, "/", escala, "/", sigla, "/output_nowcasting"),
      recursive = T,
      pattern = analise,
      full.names = TRUE
      )
  names_output <- basename(arquivos_output) %>%
    stringr::str_remove(pattern = ".csv")
  datas <-
    get.data.base(names = arquivos_output)
  output <-
    purrr::map(arquivos_output,
               data.table::fread, stringsAsFactors = FALSE)
  names(output) <-
    names_output
  output <-
    purrr::map2(.x = output,
                .y = names_output,
                ~ mutate(.x, file = .y))
  output <-
    purrr::map2(.x = output,
                .y = datas,
                ~ mutate(.x, data_analise = .y))
  all <- bind_rows(output)
  all2 <- all %>%
    mutate(
      tipo = case_when(
        str_detect(string = file, pattern = "obitos_covid")  ~ "obitos_covid",
        str_detect(string = file, pattern = "obitos_srag")  ~ "obitos_srag",
        str_detect(string = file, pattern = "srag")  ~ "srag",
        str_detect(string = file, pattern = "covid")  ~ "covid"
        ),
      sg_uf = sigla
      )
  return(data.frame(all2))
  
}

siglas<-list.files(path = "./nowcasting/Estados_betas/", recursive = FALSE, pattern = "estado_")
sigla_substrng<-substr(siglas, 8, 9)

betas_estados<-list()
for (i in sigla_substrng) {
  betas_estados[[i]]<-format_betas_output(dir = "./nowcasting/Estados_betas/estado_",
                                          escala = "estado",
                                          sigla = i,
                                          analise = c("betas_covid_2020_09_07"))
}
betas_estados<-bind_rows(betas_estados)

betas_estados_median<-list()
for (i in sigla_substrng) {
  betas_estados_median[[i]]<-format_betas_output(dir = "./nowcasting/Estados_betas_last/estado_",
                                          escala = "estado",
                                          sigla = i,
                                          analise = c("betas_obitos_covid_median_"))
}
betas_estados_median<-bind_rows(betas_estados_median)


p.cumsum<-ggplot(data = betas_estados, aes(x=atraso, y=mean))+
  geom_line(col = "black")+
  geom_ribbon(aes(ymin = lower, ymax = upper), col = "lightgrey", alpha = 0.1)+
  theme_minimal()+
  facet_wrap(sg_uf~.)
p.cumsum

prob<-rep(seq(0.25,0.95, 0.05), 27)
betas_estados_median$prob<-prob

p.median<-ggplot(data = betas_estados_median, aes(y=prob, x=mean))+
  geom_line(col = "black")+
  geom_ribbon(aes(xmin = lower, xmax = upper), col = "lightgrey", alpha = 0.1)+
  theme_minimal()+
  labs(x = "Dias",
       y = "Probilidade de Atrasos")+
  facet_wrap(sg_uf~., scale = "free")
p.median
