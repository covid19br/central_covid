library(dplyr)
library(tidyr)
library(ggplot2)

dados <- read.csv("../../dados-vacinas/doses_estados/doses_aplicadas_SP.csv")
dados %>%
    mutate(data = as.Date(data), seis.meses = (as.Date(Sys.time()) - data) > 180 ,
           esquema = recode(doses, D2 = "Completo", DU = "Completo", R = "Reforço", .default = "Incompleto"),
           idade = factor(agegroup,
                          levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", ">89"))) %>%
               filter(agegroup > 6) %>%
               group_by(seis.meses, esquema) %>%
               summarise(Doses = sum(n)) %>%
               pivot_wider(names_from = esquema, values_from = Doses)
           
