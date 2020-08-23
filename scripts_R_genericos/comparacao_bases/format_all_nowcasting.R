format_nowcasting_output <- function(dir = "dados_processados/nowcasting/",
                                         escala = "municipio",
                                         geocode = "3550308",
                                         analise = c("nowcasting_diario")) {
  path <- check.geocode(escala = escala, geocode = geocode)
  arquivos_output <-
    list.files(
      path = paste0(dir, path, "/tabelas_nowcasting_para_grafico"),
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
      )
    )
  return(data.frame(all2))

}
