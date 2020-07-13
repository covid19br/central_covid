##########plota nowcastings de uma data e notificados de uma data posterior

plot.nowcast.acumulado.check <- function(df = all2,
                                         tipo = "covid",
                                         data_inicial = c("2020_03_26"),
                                         dates_after = 3) {
  datas <- unique(df$data_analise)
  datas <- datas[c(which(datas == data_inicial),
                 which(datas == data_inicial) + dates_after)]

  dff <-  df %>%
    dplyr::filter(tipo %in% tipo) %>%
    dplyr::filter(data_analise %in% datas) %>%
    mutate(data = as.Date(data))
count(dff, tipo, data_analise)
  df1 <- dff %>% filter(data_analise == datas[1]) %>%
    dplyr::filter(tipo %in% tipo)
  df2 <- dff %>% filter(data_analise == datas[2]) %>%
    dplyr::filter(tipo %in% tipo)

  plot <- df1 %>%
    mutate(data = as.Date(data)) %>%
    ggplot(aes(x = data)) +
    # linha e ponto
    geom_line(aes(y = now.mean.c, color = "Estimados"), size = 1) +
    #geom_point(aes(y = now.mean.c, color = "Estimados"), size = 1) +
    # ic proj
    geom_ribbon(aes(ymin = now.low.c.proj, ymax = now.upp.c.proj), fill = "lightgrey") +
    # linha e ponto projecao
    geom_line(aes(y = now.mean.c.proj, color = "Estimados"), lty = "longdash") +
    geom_line(data = df2, aes(y = not.mean.c, x = data, color = "Notificados"), size = 1) +
    # ic
    geom_ribbon(data = df2, aes(ymin = not.low.c.proj, ymax = not.upp.c.proj), fill = "lightgrey") +
    # linha e ponto projecao
    geom_line(data = df2, aes(y = not.mean.c.proj, color = "Notificados"), lty = "longdash") +
    #geom_point(aes(y = not.mean.c.proj, color = "Notificados"), size = 1) +
    scale_x_date(date_labels = "%d/%b") +
    plot.formatos +
    scale_color_discrete(name = "") +
    scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")[1:2]) +
    xlab("Dia do primeiro sintoma") +
    ylab("Número acumulado de casos") +
    #theme(legend.position = "none") +
    scale_y_log10()
  out.folder <- paste0("./docs/comparacao bases/",
                       dates_after, "_days/",
                       tipo, "/")
  if (!file.exists(out.folder)) dir.create(out.folder, recursive = TRUE)
  ggsave(plot,
         filename = paste0(out.folder,
                           tipo, "_",
                           datas[1],"_", datas[2], ".png"))

}


tipo <- c("covid",
          "srag",
          "obitos_covid",
          "obitos_srag")
# execute
for (t in tipo) {
  for (i in seq_along(1:length(unique(now$data_analise)))) {
    plot_comp <-
      plot.nowcast.acumulado.check(df = all2,
                                   tipo = t,
                                   data_inicial = unique(now$data_analise)[i],
                                   dates_after = 5)
  }
}
