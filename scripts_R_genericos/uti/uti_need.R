PRJROOT <- rprojroot::find_root(".here")

# sources functions
devtools::load_all("./now_fcts/") ##loading de funções necessárias##

sivep<-read.sivep.generica("./dados/SIVEP-Gripe/SRAGHospitalizado_2020_08_31.zip")
sivep<-sivep %>% 
  filter(dt_sin_pri >= "2020-01-01" & !is.na(sg_uf) & 
           !is.na(nu_idade_n) & nu_idade_n >= 0) %>% 
  as.data.frame()

sivep<-sivep %>% 
  mutate(age_class = classifyAgeFast(as.numeric(nu_idade_n))) %>% 
  as.data.frame()

srag_series<-sivep %>% 
  group_by(dt_sin_pri, sg_uf, age_class) %>% 
  dplyr::summarize(N=n()) %>% 
  as.data.frame()
covid_series<-sivep %>% 
  filter(classi_fin == 5 | pcr_sars2 == 1) %>% 
  group_by(dt_sin_pri, sg_uf, age_class) %>% 
  dplyr::summarize(N=n()) %>% 
  as.data.frame()

covid_case_uti<-sivep %>% 
  filter(uti == 1) %>% 
  filter(classi_fin == 5 | pcr_sars2 == 1) %>% 
  select(dt_sin_pri, sg_uf, age_class) %>% 
  as.data.frame()
srag_cases_uti<-sivep %>% 
  filter(uti == 1) %>% 
  select(dt_sin_pri, sg_uf, age_class) %>% 
  as.data.frame()
covid_series_uti<-covid_case_uti %>% 
  group_by(dt_sin_pri, sg_uf, age_class) %>% 
  dplyr::summarize(N=n()) %>% 
  as.data.frame()
srag_series_uti<-srag_cases_uti %>% 
  group_by(dt_sin_pri, sg_uf, age_class) %>% 
  dplyr::summarize(N=n()) %>% 
  as.data.frame()

necessidade_uti_covid<-merge(x=covid_series,
                       y=covid_series_uti,
                       by = c("dt_sin_pri", "sg_uf", "age_class"))
names(necessidade_uti_covid)[4:5]<-c("Covid_cases", "Covid_uti")
necessidade_uti_covid<-necessidade_uti_covid %>% 
  mutate(uti_need = Covid_uti/Covid_cases) %>% 
  as.data.frame()

necessidade_uti_srag<-merge(x=srag_series,
                             y=srag_series_uti,
                             by = c("dt_sin_pri", "sg_uf", "age_class"))
names(necessidade_uti_srag)[4:5]<-c("SRAG_cases", "SRAG_uti")
necessidade_uti_srag<-necessidade_uti_srag %>% 
  mutate(uti_need = SRAG_uti/SRAG_cases) %>% 
  as.data.frame()

p.ridges.age.srag <-
  ggplot(srag_series_uti, 
         aes(x = dt_sin_pri, height = N, y = age_class, 
             fill = age_class, col = age_class)) +
  ggridges::geom_density_ridges_gradient(scale = 3, 
                                         stat = "identity", 
                                         rel_min_height = 0.01,
                                         alpha = 0.1,
                                         size = 1.2) +
  ggridges::theme_ridges() +
  scale_color_viridis_d(name = "Faixa etária", 
                        option = "viridis", 
                        aesthetics = c("colour","fill"),
                        direction = -1,
                        breaks=c("age_1", "age_2", "age_3", "age_4", "age_5", "age_6", "age_7", "age_8", "age_9"),
                        labels=c("0 a 9", "10 a 19", "20 a 29", "30 a 39",
                                 "40 a 49", "50 a 59", "60 a 69", "70 a 79", "80+")) +
  labs(caption = "Fonte: Sivep-Gripe - Elaboração @rafalpx",
       x = element_blank(),
       y = element_blank(),
       title = "Casos em UTI - SRAG",
       subtitle = "Por dia de 1º sintomas por faixa etária")+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_blank())+
  facet_wrap(~sg_uf)
p.ridges.age.srag

p.ridges.age.covid <-
  ggplot(covid_series_uti, 
         aes(x = dt_sin_pri, height = N, y = age_class, 
             fill = age_class, col = age_class)) +
  ggridges::geom_density_ridges_gradient(scale = 1, 
                                         stat = "identity", 
                                         # rel_min_height = 0.15,
                                         # alpha = 0.1,
                                         size = 1.2) +
  ggridges::theme_ridges() +
  scale_color_viridis_d(name = "Faixa etária", 
                        option = "viridis", 
                        aesthetics = c("colour","fill"),
                        direction = -1,
                        breaks=c("age_1", "age_2", "age_3", "age_4", "age_5", "age_6", "age_7", "age_8", "age_9"),
                        labels=c("0 a 9", "10 a 19", "20 a 29", "30 a 39",
                                 "40 a 49", "50 a 59", "60 a 69", "70 a 79", "80+")) +
  ggtitle("Casos em UTI") +
  labs(caption = "Fonte: Sivep-Gripe - Elaboração @rafalpx",
       x = element_blank(),
       y = element_blank(),
       title = "Casos em UTI - Covid",
       subtitle = "Por dia de 1º sintomas por faixa etária")+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_blank())+
  facet_wrap(~sg_uf)
p.ridges.age.covid

p.ridges.age.need.srag<-
  ggplot(necessidade_uti_srag, 
         aes(x = dt_sin_pri, height = uti_need, y = age_class, 
             fill = age_class, col = age_class)) +
  ggridges::geom_density_ridges_gradient(scale = 1, 
                                         stat = "identity", 
                                         # rel_min_height = 0.01,
                                         # alpha = 0.1,
                                         size = 1.2) +
  ggridges::theme_ridges() +
  scale_color_viridis_d(name = "Faixa etária", 
                        option = "viridis", 
                        aesthetics = c("colour","fill"),
                        direction = -1
                        # breaks=c("age_1", "age_2", "age_3", "age_4", "age_5", "age_6", "age_7", "age_8", "age_9"),
                        # labels=c("0 a 9", "10 a 19", "20 a 29", "30 a 39",
                        #          "40 a 49", "50 a 59", "60 a 69", "70 a 79", "80+")
                        ) +
  labs(caption = "Fonte: Sivep-Gripe - Elaboração @rafalpx",
       x = element_blank(),
       y = element_blank(),
       title = "Necessidade de UTI - SRAG",
       subtitle = "Por dia de 1º sintomas por faixa etária")+
  theme(legend.position = "none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_blank())+
  facet_wrap(~sg_uf, ncol = 3)
p.ridges.age.need.srag

p.ridges.age.need.covid<-
  ggplot(necessidade_uti_covid, 
         aes(x = dt_sin_pri, y = uti_need, height = Covid_cases,
             fill = uti_need, col = uti_need)) +
  geom_ridgeline(position = "fill")+
  theme_classic()+
  scale_color_viridis_c(name = "ICU Need",
                        option = "viridis",
                        aesthetics = c("colour","fill"),
                        direction = -1) +
  labs(caption = "Fonte: Sivep-Gripe - Elaboração @rafalpx",
       x = element_blank(),
       y = element_blank(),
       title = "Necessidade de UTI - Covid",
       subtitle = "Por dia de 1º sintomas por faixa etária")+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_blank())+
  facet_grid(sg_uf ~ age_class)
p.ridges.age.need.covid

p.arrange<-ggarrange(p.ridges.age.need.srag,
                     p.ridges.age.need.covid, 
                     common.legend = TRUE,
                     legend = "bottom")
p.arrange

necessidade_uti_srag_SP<-necessidade_uti_srag %>% 
  filter(sg_uf == "SP") %>% 
  as.data.frame()
necessidade_uti_covid_SP<-necessidade_uti_covid %>% 
  filter(sg_uf == "SP") %>% 
  as.data.frame()
diff_need_uti_sp<-merge(x=necessidade_uti_covid_SP[,c("dt_sin_pri", "sg_uf", "age_class", "uti_need")],
                        y=necessidade_uti_srag_SP[,c("dt_sin_pri", "sg_uf", "age_class", "uti_need")],
                        by = c("dt_sin_pri", "sg_uf", "age_class"))
diff_need_uti_sp <- diff_need_uti_sp %>% 
  mutate(uti_need_covid = uti_need.x,
         uti_need_srag = uti_need.y,
         diff_srag_covid = uti_need_srag - uti_need_covid) %>% 
  as.data.frame()

p.test<-ggplot(diff_need_uti_sp, aes(x = dt_sin_pri, 
                                            y = age_class, 
                                            fill = age_class, 
                                            height = diff_srag_covid)) +
  geom_density_ridges(scale = 5, stat = "identity") +
  # geom_density_ridges2(data = necessidade_uti_covid_SP, 
  #                      aes(x=dt_sin_pri, y=age_class, fill = age_class, height = uti_need), 
  #                      scale = 5, stat = "identity")+
  theme_get()+
  # scale_y_discrete(expand = c(0.01, 0)) +
  # scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_brewer(palette = 4) +
  theme_ridges() + theme(legend.position = "none")
p.test
# p.uol <-
#   ggplot(srag_series_uti, aes(x = dt_sin_pri , y = N)) +
#   geom_line( aes(col = "SRAG")) +
#   geom_line(data = covid_series_uti, aes(x = dt_sin_pri, y = N, color = "Covid"))+
#   labs(x = "Data 1º sintomas" , y = "Número de Casos UTI ") +
#   theme_bw() +
#   theme(legend.position = "right") +
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 14))+
#   scale_color_manual(values = c("blue", "red"),  aesthetics = "colour")+
#   facet_wrap(~sg_uf, scales = "free")
# p.uol
# 
# p.uol_need_uti<-
#   ggplot(necessidade_uti_covid, aes(x = dt_sin_pri , y = uti_need)) +
#   geom_line(aes(col = "Covid")) +
#   geom_line(data = necessidade_uti_srag, aes(x = dt_sin_pri, y = uti_need, color = "SRAG"))+
#   labs(x = "Data 1º sintomas" , y = "", 
#        title = "Necessidade de UTI(Casos UTI/Casos)") +
#   theme_bw() +
#   theme(legend.position = "right", legend.title = element_text("Casos")) +
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 14))+
#   scale_color_manual(values = c("blue", "red"),  aesthetics = "colour")+
#   facet_wrap(~sg_uf)
# p.uol_need_uti