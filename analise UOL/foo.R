library("ggplot2")
library("cowplot")
uol_final_df <- read.csv("~/Área de Trabalho/Análise UOL/uol_nowcasting_final.csv", as.is = TRUE)
# uol_final_df2 <- read_excel("~/Área de Trabalho/Análise UOL/uol_nowcasting_final2.xlsx")

uol.df <- reshape2::melt(uol_final_df[, c("Data", "uol_zoo", "estimate.nowcasting.10", "estimate.nowcasting.20")], id.vars = "Data")
# uol.df <- reshape2::melt(uol_final_df2[, c("Dados Oficias", "Acrescido pelo nowcasting 10 dias", "Acrescido pelo nowcasting 20 dias", "Data")], id.vars = "Data")

uol.error <- data.frame(Data = rep(uol_final_df$Data, 3),
                        lower = c(uol_final_df$lower.nowcasting.10,
                                  uol_final_df$lower.nowcasting.20,
                                  rep(NA, nrow(uol_final_df))),
                        upper = c(uol_final_df$upper.nowcasting.10,
                                  uol_final_df$upper.nowcasting.20,
                                  rep(NA, nrow(uol_final_df))),
                        source = rep(c("now10", "now20", "off"), each = nrow(uol_final_df)))
uol.error$lower[uol.error$lower == 0] <- NA
uol.error$upper[uol.error$upper == 0] <- NA

p.final.ic <-
    ggplot(uol.df) +
    geom_col(aes(x = as.Date(Data, format = "%Y-%m-%d"), y = value, fill = variable) , position = "dodge") +
    # geom_errorbar(data = uol.error, aes(x = as.Date(Data, format = "%Y-%m-%d"), ymin = lower, ymax = upper, colour = factor(source, levels = c("off", "now10", "now20"))), position = "dodge", size = 1.5) +
    scale_fill_manual(values = c("black", "indianred3",  "skyblue"), name = "Legenda", labels = c("Dados oficiais", "Estimativa 10 dias", "Estimativa 20 dias")) +
    scale_colour_manual(values = c("black", "indianred3",  "skyblue"), name = "Legenda", labels = c("Dados oficiais", "Estimativa 10 dias", "Estimativa 20 dias")) +
    labs(x = "Data", y = "Número de Óbitos") +
    theme_cowplot() +
    theme(legend.position = c(0.1, 0.8))
p.final.ic

ggsave(filename = "~/Área de Trabalho/Análise UOL/p_final_uol_dogded.pdf", p.final.ic, dpi = 600, width = 9, height = 7)
ggsave(filename = "~/Área de Trabalho/Análise UOL/p_final_uol_dogded.png", p.final.ic, dpi = 600, width = 9, height = 7)

uol_final_df2 <- read.csv("~/Área de Trabalho/Análise UOL/uol_nowcasting_final2.csv", as.is = TRUE)
uol.df2 <- reshape::melt(uol_final_df2, id.vars = "Data")
uol.df2$Data <- as.Date(uol.df2$Data, format = "%Y-%m-%d")
uol.df2$variable <- factor(uol.df2$variable, levels = rev(levels(uol.df2$variable)))

p.final.stack <- 
    ggplot(uol.df2) +
    geom_col(aes(x = Data, y = value, fill = variable), position = "stack") +
    scale_fill_manual(values = c("skyblue", "indianred3", "black"), name = "Legenda", labels = c("Acréscimo com janela de 20 dias", "Acréscimo com janela de 10 dias", "Dados oficiais")) +
    labs(x = "Data", y = "Número de Óbitos") +
    theme_cowplot() +
    theme(legend.position = c(0.1, 0.8))
p.final.stack
ggsave(filename = "~/Área de Trabalho/Análise UOL/p_final_uol_stacked.pdf", p.final.stack, dpi = 600, width = 9, height = 7)
ggsave(filename = "~/Área de Trabalho/Análise UOL/p_final_uol_stacked.png", p.final.stack, dpi = 600, width = 9, height = 7)

    