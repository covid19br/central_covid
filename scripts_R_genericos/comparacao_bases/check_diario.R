##########plota nowcastings de uma data e notificados de uma data posterior


plot.nowcast.diario.check <- function(df = all2,
                                      tipo = "covid",
                                      data_inicial = c("2020_03_26"),
                                      data_final = c("2020_07_07"),
                                      dates_after = 7) {
  datas <- unique(df$data_analise)
  datas <- datas[c(which(datas == data_inicial),
                 which(datas == data_inicial) + dates_after)]
  dff <-  df %>%
    dplyr::filter(data_analise %in% datas) %>%
    dplyr::filter(tipo %in% tipo) %>%
    mutate(data = as.Date(data))

  antes <- dff %>% filter(data_analise == datas[1])
  depois <- dff %>% filter(data_analise == datas[2])
names(df1)
  plot <- antes %>%
    mutate(data = as.Date(data)) %>%
    ggplot(aes(x = data)) +
    geom_line(aes(y = estimate.merged.smooth), lty = 2, col = "grey") +
    #geom_point(aes(y = estimate, col = "Nowcasting"), size = 2) +
    geom_point(aes(y = n.casos, col = "Notificado"), size = 2) +
    geom_ribbon(aes(ymin = lower.merged.pred, ymax = upper.merged.pred),
                fill = RColorBrewer::brewer.pal(3, "Set1")[2], alpha = 0.1) +
    #geom_line(aes(y = estimate.merged.smooth), alpha = 0.6, size = 2) +
    geom_point(data = antes, aes(y = n.casos, x = data)) +
    scale_x_date(date_labels = "%d/%b") +
    scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")[1:3]) +
    xlab("Dia do primeiro sintoma") +
    ylab("Número de novos casos") +
    #plot.formatos +
    theme(legend.position = "none")
  plot
}
