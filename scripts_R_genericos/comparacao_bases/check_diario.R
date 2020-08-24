compara_nowcastings_diarios <- function(df = all2,
                                        tipo_caso = "covid",
                                        data_inicial = c("2020_03_26"),
                                        data_final = c("2020_07_07"),
                                        dates_after = 7,
                                        plot.points = TRUE) {
    if (missing(data_final) & !missing(dates_after)) {
        datas <- unique(df$data_analise)
        data_final <- datas[which(datas == data_inicial) + dates_after]
    }
    datas <- c(data_inicial, data_final)
    
    dff <-  df %>%
        dplyr::filter(tipo == tipo_caso) %>%
        dplyr::filter(data_analise %in% datas) %>%
        mutate(data = as.Date(data))
    antes <- dff %>% filter(data_analise == datas[1])
    depois <- dff %>% filter(data_analise == datas[2])
    end <- depois$data[which(is.na(depois$n.casos))[1]]
    depois <- depois %>% filter(data < end)
    
  plot <- antes %>%
      mutate(data = as.Date(data)) %>%
      ggplot(aes(x = data)) +
      ##geom_point(aes(y = n.casos, col = paste("Notificados", data[1])), size = 2) +
      geom_ribbon(aes(ymin = lower.merged.pred, ymax = upper.merged.pred),
                  fill = RColorBrewer::brewer.pal(3, "Set2")[1], alpha = 0.5) +
      geom_line(aes(y = estimate.merged.smooth, col = paste("Nowcasting", datas[1])), alpha = 0.6, size = 1.5) +
      geom_line(data = depois, aes(y = estimate.merged.smooth, x = data, col = paste("Número de casos", datas[2])),
                alpha = 0.6, size = 1.2) +
      ##geom_point(data = depois, aes(y = n.casos, col = paste("Número de casos", datas[2])), size = 2) +
      ##geom_point(data = antes, aes(y = n.casos, col = paste("Nowcasting", datas[1])), size = 2) +
      geom_ribbon(data = depois, aes(ymin = lower.merged.pred, ymax = upper.merged.pred),
                  fill = RColorBrewer::brewer.pal(3, "Set2")[2], alpha = 0.5) +
      scale_x_date(date_labels = "%d/%b") +
      scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set2")[1:3]) +
      xlab("Dia do primeiro sintoma") +
      ylab("Número de novos casos") +
      theme_classic() +
      theme(legend.position = c(0.2, 0.8),
          legend.title = element_blank()) +
      geom_vline(xintercept = antes$data[which(is.na(antes$n.casos))[1] - 1]) +
      geom_vline(xintercept = max(antes$data))
    if(plot.points) {
        plot +
            geom_point(data = depois, aes(y = n.casos), col = "red", size = 2) +
            geom_point(data = antes, aes(y = n.casos), col = RColorBrewer::brewer.pal(3, "Set2")[1], size = 2)
        }
    else
        plot
}
