###############
### Diadema ###
###############
  
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(NobBS)){install.packages("NobBS"); library(NobBS)}
if(!require(rprojroot)){install.packages("rprojroot"); library(rprojroot)}
if(!require(readxl)){install.packages("readxl"); library(readxl)}
if(!require(textclean)){install.packages("textclean"); library(textclean)}
if(!require(ggpubr)){install.packages("ggpubr"); library(ggpubr)}

devtools::load_all("./now_fcts/R/") ##loading de funções necessárias##

PRJROOT  = rprojroot::find_root(".here")

diadema<-read_csv("scripts_R_genericos/Diadema/diadema.csv")
names(diadema)<-tolower(names(diadema))

diadema<-diadema %>% 
  filter(!is.na(data_pri_sin) & !is.na(data_notificacao)) %>% 
  mutate(data_da_coleta = as.Date(data_da_coleta, "%d/%m/%Y"),
         data_pri_sin = as.Date(data_pri_sin, "%d/%m/%Y"),
         data_do_obito = as.Date(data_do_obito, "%d/%m/%Y"),
         data_notificacao = as.Date(data_notificacao, "%d/%m/%Y"),
         atraso = as.numeric(data_notificacao - data_pri_sin)) %>% 
  filter(atraso >= 0) %>% 
  as.data.frame()


plot(density(diadema$atraso, na.rm = TRUE), 
     xlab = "Atraso entre 1º sintomas e notificação",
     main = "Densidade de atrasos"
)

diadema_sin_pri<-diadema %>% 
  group_by(data_pri_sin) %>% 
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()
diadema_not<-diadema %>% 
  group_by(data_notificacao) %>% 
  dplyr::summarise(N=n())%>%
  mutate(Cum=cumsum(N))%>%
  as.data.frame()

p.plot<-ggplot(data = diadema_sin_pri, aes(x=data_pri_sin, y = N))+
  geom_line(col = "red")+
  geom_line(data = diadema_not, aes(x=data_notificacao, y=N), col = "blue")+
  xlab("Datas") +
  ylab("Nº Casos") +
  theme_bw()+
  theme(legend.position="none") +
  ggtitle("Diadema - SP")
p.plot

diadema_datas<-diadema %>% 
  filter(!is.na(data_pri_sin) & !is.na(data_notificacao) & atraso >= 0) %>% 
  mutate(data_pri_sin = as.Date(data_pri_sin, format = "%Y-%m-%d"),
         data_notificacao = as.Date(data_notificacao, format = "%Y-%m-%d")) %>% 
  select(data_pri_sin, data_notificacao) %>% 
  as.data.frame()

# write.csv(diadema_datas, file = "./scripts_R_genericos/Diadema/diadema_datas.csv", row.names = FALSE)
# diadema_datas<-read_csv("./scripts_R_genericos/Diadema/diadema_datas.csv")

nowcasting_diadema<-NobBS(data = diadema_datas,
                          now = max(diadema_datas$data_pri_sin),
                          onset_date = "data_pri_sin",
                          report_date = "data_notificacao",
                          units = "1 day",
                          moving_window = 60,
                          specs = list(nAdapt = 5000, nBurnin = 3000, nThin = 1, nSamp = 10000))
betas_diadema<-beta.summary(nowcasting_diadema) #### função em funcoes.R

p.betas.diadema<-
  ggplot(betas_diadema, aes(atraso, mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  xlab("Dias após 1º sintomas") +
  ylab("Probabilidade de notificação") +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Atraso de notificação Diários")
p.betas.diadema

ggsave(p.betas.diadema, filename = paste0("./scripts_R_genericos/Diadema/betas_diadema.png"),
       dpi = 600, width = 9, height = 7)

p.prev.ic_diadema<-nowcasting_diadema$estimates %>%
  ggplot(aes(x = onset_date, y = estimate)) +
  geom_line(data = diadema_sin_pri, aes(x = data_pri_sin, y = N, color="Notificados"), lwd = 0.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Data 1º sintomas") +
  ylab("Nº de Casos por dia") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8), legend.title= element_blank()) +
  scale_colour_manual(values = c("red", "dark orange"), aesthetics = c("colour", "fill"))+
  ggtitle("Casos Diários")
p.prev.ic_diadema

ggsave(p.prev.ic_diadema, filename = paste0("./scripts_R_genericos/Diadema/nowcasting_diadema.png"),
       dpi = 600, width = 9, height = 7)

diadema_sin_pri2<-diadema_sin_pri %>% 
  filter(data_pri_sin >= min(nowcasting_diadema$estimates$onset_date)) %>% 
  as.data.frame()

p.prev.ic_diadema2<-nowcasting_diadema$estimates %>%
  ggplot(aes(x = onset_date, y = estimate)) +
  geom_line(data = diadema_sin_pri2, aes(x = data_pri_sin, y = N, color="Notificados"), lwd = 0.5) +
  geom_line(aes(col = "Estimado")) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill="red", alpha =0.15) +
  xlab("Data 1º sintomas") +
  ylab("Nº de Casos por dia") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("red", "dark orange"), aesthetics = c("colour", "fill"))+
  ggtitle("Casos Diários")
p.prev.ic_diadema2

ggsave(p.prev.ic_diadema2, filename = paste0("./scripts_R_genericos/Diadema/nowcasting_diadema_zoom.png"),
       dpi = 600, width = 9, height = 7)

arrange<-ggarrange(p.prev.ic_diadema, p.prev.ic_diadema2)
arrange

ggsave(arrange, filename = paste0("./scripts_R_genericos/Diadema/arrange_diadema_zoom.png"),
       dpi = 600, width = 9, height = 7)
