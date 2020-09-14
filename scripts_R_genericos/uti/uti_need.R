library(tidyverse)
library(ggpubr)

PRJROOT <- rprojroot::find_root(".here")

# sources functions
devtools::load_all("./now_fcts/") ##loading de funções necessárias##

sivep<-read.sivep.generica("./dados/SIVEP-Gripe/SRAGHospitalizado_2020_09_07.zip")
sivep<-sivep %>% 
  filter(dt_sin_pri >= "2020-01-01" & !is.na(sg_uf) & hospital == 1 & 
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
         aes(x = dt_sin_pri, y = SRAG_cases,
             fill = uti_need, group = age_class)) +
  geom_col(width = 1, na.rm = TRUE, position = position_stack(reverse = TRUE))+
  theme_classic()+
  scale_fill_viridis_c(name = "ICU Need",
                        option = "viridis",
                        aesthetics = c("fill"),
                        direction = -1) +
  labs(title = "ICU Need - SRAG",
    x = element_blank(),
    y = element_blank())+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_wrap(sg_uf ~., scales = "free_y", ncol = 3)
p.ridges.age.need.srag

p.ridges.age.need.covid<-
  ggplot(necessidade_uti_covid, 
         aes(x = dt_sin_pri, y = Covid_cases,
             fill = uti_need, group = age_class)) +
  geom_col(width = 1, na.rm = TRUE, position = position_stack(reverse = TRUE))+
  theme_classic()+
  scale_fill_viridis_c(name = "ICU Need",
                        option = "viridis",
                        aesthetics = c("fill"),
                        direction = -1) +
  labs(title = "ICU Need - Covid",
    x = element_blank(),
    y = element_blank())+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_wrap(sg_uf~., scales = "free_y", ncol = 3)
p.ridges.age.need.covid

p.arrange<-ggarrange(p.ridges.age.need.srag,
                     p.ridges.age.need.covid, common.legend = TRUE, legend = "bottom")
p.arrange

###################
####  DEATHS ######
###################

sivep_obs<-sivep %>% 
  filter(evolucao == 2) %>% 
  as.data.frame()

srag_series_obs<-sivep_obs %>% 
  group_by(dt_sin_pri, sg_uf, age_class) %>% 
  dplyr::summarize(N=n()) %>% 
  as.data.frame()
covid_series_obs<-sivep %>% 
  filter(classi_fin == 5 | pcr_sars2 == 1) %>% 
  group_by(dt_sin_pri, sg_uf, age_class) %>% 
  dplyr::summarize(N=n()) %>% 
  as.data.frame()

covid_case_uti_obs<-sivep_obs %>% 
  filter(uti == 1) %>% 
  filter(classi_fin == 5 | pcr_sars2 == 1) %>% 
  select(dt_sin_pri, sg_uf, age_class) %>% 
  as.data.frame()
srag_cases_uti_obs<-sivep_obs %>% 
  filter(uti == 1) %>% 
  select(dt_sin_pri, sg_uf, age_class) %>% 
  as.data.frame()
covid_series_uti_obs<-covid_case_uti_obs %>% 
  group_by(dt_sin_pri, sg_uf, age_class) %>% 
  dplyr::summarize(N=n()) %>% 
  as.data.frame()
srag_series_uti_obs<-srag_cases_uti_obs %>% 
  group_by(dt_sin_pri, sg_uf, age_class) %>% 
  dplyr::summarize(N=n()) %>% 
  as.data.frame()

necessidade_uti_covid_obs<-merge(x=covid_series_obs,
                             y=covid_series_uti_obs,
                             by = c("dt_sin_pri", "sg_uf", "age_class"))
names(necessidade_uti_covid_obs)[4:5]<-c("Covid_deaths", "Covid_deaths_uti")
necessidade_uti_covid_obs<-necessidade_uti_covid_obs %>% 
  mutate(uti_need = Covid_deaths_uti/Covid_deaths) %>% 
  as.data.frame()

necessidade_uti_srag_obs<-merge(x=srag_series_obs,
                            y=srag_series_uti_obs,
                            by = c("dt_sin_pri", "sg_uf", "age_class"))
names(necessidade_uti_srag_obs)[4:5]<-c("SRAG_deaths", "SRAG_deaths_uti")
necessidade_uti_srag_obs<-necessidade_uti_srag_obs %>% 
  mutate(uti_need = SRAG_deaths_uti/SRAG_deaths) %>% 
  as.data.frame()

p.ridges.age.need.srag.deaths<-
  ggplot(necessidade_uti_srag_obs, 
         aes(x = dt_sin_pri, y = SRAG_deaths,
             fill = uti_need, group = age_class)) +
  geom_col(width = 1, na.rm = TRUE, position = position_stack(reverse = TRUE))+
  theme_classic()+
  scale_fill_viridis_c(name = "ICU Need",
                       option = "viridis",
                       aesthetics = c("fill"),
                       direction = -1) +
  labs(title = "ICU Need - SRAG (Deaths)",
       x = element_blank(),
       y = element_blank())+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_wrap(sg_uf ~., scales = "free_y", ncol = 3)
p.ridges.age.need.srag.deaths

p.ridges.age.need.covid.deaths<-
  ggplot(necessidade_uti_covid_obs, 
         aes(x = dt_sin_pri, y = Covid_deaths,
             fill = uti_need, group = age_class)) +
  geom_col(width = 1, na.rm = TRUE, position = position_stack(reverse = TRUE))+
  theme_classic()+
  scale_fill_viridis_c(name = "ICU Need",
                       option = "viridis",
                       aesthetics = c("fill"),
                       direction = -1) +
  labs(title = "ICU Need - Covid (Deaths)",
       x = element_blank(),
       y = element_blank())+
  theme(legend.position = "bottom",
        panel.spacing = unit(0.01, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14))+
  facet_wrap(sg_uf~., scales = "free_y", ncol = 3)
p.ridges.age.need.covid.deaths

p.arrange.deaths<-ggarrange(p.ridges.age.need.srag.deaths,
                     p.ridges.age.need.covid.deaths, common.legend = TRUE, legend = "bottom")
p.arrange.deaths

############
### SRAG ###
############
p.arrange.srag<-ggarrange(p.ridges.age.need.srag, p.ridges.age.need.srag.deaths, common.legend = TRUE, legend = "bottom")
p.arrange.srag

p.arrange.covid<-ggarrange(p.ridges.age.need.covid, p.ridges.age.need.covid.deaths, common.legend = TRUE, legend = "bottom")
p.arrange.covid

uti_need_srag<-merge(x=necessidade_uti_srag,
                      y=necessidade_uti_srag_obs,
                      by = c("dt_sin_pri", "sg_uf", "age_class"))
names(uti_need_srag)[c(6,9)]<-c("uti_need_cases", "uti_need_deaths")

uti_need_covid<-merge(x=necessidade_uti_covid,
                      y=necessidade_uti_covid_obs,
                      by = c("dt_sin_pri", "sg_uf", "age_class"))
names(uti_need_covid)[c(6,9)]<-c("uti_need_cases", "uti_need_deaths")
