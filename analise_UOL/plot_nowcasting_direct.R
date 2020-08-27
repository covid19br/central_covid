#############################
######### Gráficos ##########
# #############################
sivep_direct<-read_csv("./analise_UOL/dados/extract_dates_sivep_03_08.csv")
trim<-0
last_data<-max(sivep_direct$dt_evoluca)
ifelse(trim>0,
       trimmed<-paste0("_trimmed_",trim),trimmed<-NULL)
# 
# 
# ## Data Evolução ##
nowcasting_direct_estimates<-read_csv(paste0("./analise_UOL/output/nowcasting_estimates_direct_", last_data,trimmed,".csv"))
betas_direct<-read_csv(paste0("./analise_UOL/output/betas_direct_", last_data,trimmed,".csv"))
betas_cumsum_direct<-read_csv(paste0("./analise_UOL/output/betas_cumsum_direct_",last_data,trimmed,".csv"))
nowcasting_cumsum_direct<-read_csv(paste0("./analise_UOL/output/nowcasting_cumsum_direct_", last_data,trimmed,".csv"))
median_delays<-quantile_delay(as.data.frame(betas_cumsum_direct), prob = seq(0.1,0.99, 0.05))

# ################################################################################
# ## Plots: objetos ggplot2
# ################################################################################
p.betas.direct<-
  ggplot(betas_direct, aes(atraso, mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  xlab("Dias após dia do óbito") +
  ylab("Probabilidade de notificação") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Atraso de notificação Diários")
p.betas.direct

p.betas_cumsum_direct <-
  ggplot(betas_cumsum_direct, aes(atraso, mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  xlab("Dias após dia do óbito") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Atraso de notificação Acumulado")+
  ylab("Probabilidade Acumulada\n de notificação")
p.betas_cumsum_direct

p.prev.ic_direct<-nowcasting_direct$estimates %>%
  ggplot(aes(x = onset_date, y = estimate)) +
  geom_line(data = sivep_series, aes(x = dt_evoluca, y = N, color="Notificados"), lwd = 1.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos por dia") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "dark orange"), aesthetics = c("colour", "fill"))+
  ggtitle("Óbitos Diários")
p.prev.ic_direct

p.prev.ic.cumsum_direct<-
  ggplot(nowcasting_cumsum_direct, aes(x= Dates, y = mean))+
  geom_line(data = sivep_series, aes(x = dt_evoluca, y = Cum, color="Notificados"), lwd = 1.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Dia do Óbito") +
  ylab("Nº de Óbitos Acumulados") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "dark orange"), aesthetics = c("colour", "fill"))+
  ggtitle("Óbitos Acumulados")
p.prev.ic.cumsum_direct

p.arrange<-ggpubr::ggarrange(p.betas.direct, p.betas_cumsum_direct, p.prev.ic_direct, p.prev.ic.cumsum_direct)
p.arrange
ggsave(p.arrange, filename = paste0("./analise_UOL/plots/arrange_nowcasting_",last_data,".png"),
       dpi = 600, width = 9, height = 7)

