library("tidyverse")
library("cowplot")
library("RColorBrewer")

betas_29<-read_csv(paste0("./analise_UOL/output/betas_cumsum_direct_2020-07-29.csv"))
betas_21<-read_csv(paste0("./analise_UOL/output/betas_cumsum_direct_2020-07-21.csv"))
betas_14<-read_csv(paste0("./analise_UOL/output/betas_cumsum_direct_2020-07-14.csv"))

pontos <- data.frame(
    y = betas_29$mean[c(1, 11, 16, 31, 61)],
    x = betas_29$atraso[c(1, 11, 16, 31, 61)]
    )

cols <- rev(brewer.pal(5, "Set1"))

# p.betascumsum <-
    ggplot(betas_29) +
    geom_line(aes(x = atraso, y = 100 * mean), size = 1.2, col = "red") +
    geom_ribbon(aes(x = atraso, ymin = 100 * lower, ymax = 100 * upper), alpha = 0.1, fill = "red") +
    geom_segment(aes(x = pontos$x[1], xend = pontos$x[1], y = 0, yend = 100 * pontos$y[1]), linetype = "dotted", colour = cols[1], size = 0.5,) +
    geom_segment(aes(x = pontos$x[2], xend = pontos$x[2], y = 0, yend = 100 * pontos$y[2]), linetype = "dotted", colour = cols[2], size = 0.5) +
    geom_segment(aes(x = pontos$x[3], xend = pontos$x[3], y = 0, yend = 100 * pontos$y[3]), linetype = "dotted", colour = cols[3], size = 0.5) +
    geom_segment(aes(x = pontos$x[4], xend = pontos$x[4], y = 0, yend = 100 * pontos$y[4]), linetype = "dotted", colour = cols[4], size = 0.5) +
    geom_segment(aes(x = pontos$x[5], xend = pontos$x[5], y = 0, yend = 100 * pontos$y[5]), linetype = "dotted", colour = cols[5], size = 0.5) +
    # geom_segment(aes(x = 0, xend = pontos$x[1], y = 100 * pontos$y[1], yend = 100 * pontos$y[1]), linetype = "dotted", colour = cols[1], size = 0.5) +
    # geom_segment(aes(x = 0, xend = pontos$x[2], y = 100 * pontos$y[2], yend = 100 * pontos$y[2]), linetype = "dotted", colour = cols[2], size = 0.5) +
    # geom_segment(aes(x = 0, xend = pontos$x[3], y = 100 * pontos$y[3], yend = 100 * pontos$y[3]), linetype = "dotted", colour = cols[3], size = 0.5) +
    # geom_segment(aes(x = 0, xend = pontos$x[4], y = 100 * pontos$y[4], yend = 100 * pontos$y[4]), linetype = "dotted", colour = cols[4], size = 0.5) +
    # geom_segment(aes(x = 0, xend = pontos$x[5], y = 100 * pontos$y[5], yend = 100 * pontos$y[5]), linetype = "dotted", colour = cols[5], size = 0.5) +
    geom_text(data = pontos, aes(x = x + c(5, 5, 4), y = (y * 100) - 4, label = paste0(round(y, 3) * 100, "%"), fontface = "bold"), size = 8, colour = cols) +
    geom_point(data = pontos, aes(x = x, y = y * 100), size = 4, colour = cols) +
    labs(x = "Dias após dia do óbito", y = "Probabilidade Acumulada\nde notificação",
         title = "Atraso de notificação de óbitos COVID por SRAG", 
         subtitle = paste0("Dados SIVEP-Gripe 29/07/2020"),
         caption = "Elaboração: @rafalpx, Observatório COVID19 BR; covid19br.github.io") +
    theme(legend.position = "none") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 18), 
          axis.title = element_text(size = 22), 
          axis.text.y = element_text(size = 18), 
          plot.title = element_text(size = 25, hjust = 0.5))
# p.betascumsum
p.betascumsum %+% betas_21
# p.betascumsum %+% betas_14
ggsave(p.betascumsum, filename = paste0("./analise_UOL/plots/betas_acumulados_",last_data,".png"), 
       dpi = 600, width = 9, height = 7)
