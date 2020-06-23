library("tidyverse")
library("cowplot")
library("RColorBrewer")

# uol_betas3 <- read.csv("./dados/uol_final_betas_03_06_cumsum.csv", as.is = TRUE)

uol_betas3<-betas_maxd_cumsum

uol_betas3$upper[uol_betas3$upper > 1] <- 1

pontos <- data.frame(
    y = uol_betas3[c(11, 31, 40, 61), "mean"],
    x = uol_betas3$atraso[c(11, 31, 40, 61)]
    )

cols <- rev(brewer.pal(4, "Set1"))

#p.betascumsum <-

    ggplot(uol_betas3) +
    geom_line(aes(x = atraso, y = 100 * mean), size = 1.2, col = "skyblue") +
    geom_ribbon(aes(x = atraso, ymin = 100 * lower, ymax = 100 * upper), alpha = 0.1, fill = "blue") +
    geom_segment(aes(x = pontos$x[1], xend = pontos$x[1], y = 0, yend = 100 * pontos$y[1]), linetype = "dotted", colour = cols[1], size = 0.5,) +
    geom_segment(aes(x = pontos$x[2], xend = pontos$x[2], y = 0, yend = 100 * pontos$y[2]), linetype = "dotted", colour = cols[2], size = 0.5) +
    geom_segment(aes(x = pontos$x[3], xend = pontos$x[3], y = 0, yend = 100 * pontos$y[3]), linetype = "dotted", colour = cols[3], size = 0.5) +
    geom_segment(aes(x = pontos$x[4], xend = pontos$x[4], y = 0, yend = 100 * pontos$y[4]), linetype = "dotted", colour = cols[4], size = 0.5) +
    geom_segment(aes(x = 0, xend = pontos$x[1], y = 100 * pontos$y[1], yend = 100 * pontos$y[1]), linetype = "dotted", colour = cols[1], size = 0.5) +
    geom_segment(aes(x = 0, xend = pontos$x[2], y = 100 * pontos$y[2], yend = 100 * pontos$y[2]), linetype = "dotted", colour = cols[2], size = 0.5) +
    geom_segment(aes(x = 0, xend = pontos$x[3], y = 100 * pontos$y[3], yend = 100 * pontos$y[3]), linetype = "dotted", colour = cols[3], size = 0.5) +
    geom_segment(aes(x = 0, xend = pontos$x[4], y = 100 * pontos$y[4], yend = 100 * pontos$y[4]), linetype = "dotted", colour = cols[4], size = 0.5) +
    geom_text(data = pontos, aes(x = x + c(5, 5, 4), y = (y * 100) - 4, label = paste0(round(y, 3) * 100, "%"), fontface = "bold"), size = 8, colour = cols) +
    geom_point(data = pontos, aes(x = x, y = y * 100), size = 4, colour = cols) +
    labs(x = "Dias após dia do óbito", y = "Probabilidade Acumulada\nde notificação") +
    theme(legend.position = "none") +
    ggtitle("Atraso de notificação de óbitos COVID por SRAG") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 18), axis.title = element_text(size = 22), axis.text.y = element_text(size = 18), plot.title = element_text(size = 25, hjust = 0.5))

p.betascumsum
