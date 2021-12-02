library(tidyverse)
library(lubridate)

uf <- read.csv('../../dados/estadosBR.csv')
## url <- 'https://raw.githubusercontent.com/covid19br/dados-vacinas/main/doses_estados/doses_aplicadas_XXX.csv'
url <- "../../dados-vacinas/doses_estados/doses_aplicadas_XXX.csv"
    
consome <- function(x,u=url) {
  url2 <- str_replace(u,'XXX',x)
  read_csv(url2)
}


vacinas <- uf$UF %>% map_df(consome)

## Curvas cumulativas
dados <-
    vacinas %>%
    mutate(data = as.Date(data),
           esquema = recode(doses, D2 = "Completo", DU = "Completo", R = "Reforço", .default = "Incompleto"),
           idade = factor(agegroup,
                          labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", ">89"))) %>%
    group_by(data, idade, esquema) %>%
    summarise(Doses = sum(n)) %>%
    group_by(idade,esquema) %>%
    mutate(DosesC=cumsum(Doses))

dados$DosesC <- with(dados, ave(Doses, idade, esquema, FUN = cumsum))

p1 <-
    dados %>%
    filter(!is.na(idade)) %>%
    ggplot(aes(data,DosesC)) +
    geom_line(aes(colour=esquema), size=1.2) +
    facet_wrap(~idade, scales="free") +
    theme_bw()
png("BR_esquema_idades.png", width=900, height=400)
print(p1)
dev.off()


## Tabela meio ruim de quantos com esquema completo há seis meses ou mais
vacinas %>%
    mutate(data = as.Date(data), seis.meses = (as.Date(Sys.time()) - data) > 180 ,
           esquema = recode(doses, D2 = "Completo", DU = "Completo", R = "Reforço", .default = "Incompleto"),
           idade = factor(agegroup,
                          levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", ">89"))) %>%
               filter(agegroup > 6) %>%
               group_by(seis.meses, esquema) %>%
               summarise(Doses = sum(n)) %>%
               pivot_wider(names_from = esquema, values_from = Doses)
           
