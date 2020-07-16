library(tibble)
library(reshape)
library(tidyr)
library(splitstackshape)
library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(R.utils)
library(readr)

mun.name <- "Florianópolis"
munFileName <-  paste0("./scripts_R_genericos/summary_esus/Florianopolis/summary_esus_", mun.name,".csv")

tudo <- read.csv(
  munFileName,
  col.names = c("datainiciosintomas", "n.casos", "data.base")) %>%
  arrange(datainiciosintomas, data.base) %>%
  filter(!is.na(datainiciosintomas))

#filtra a primeira aparição do caso
data_wide <- tudo %>%
  distinct(datainiciosintomas, n.casos, .keep_all = TRUE)
data_wide <-
  tidyr::pivot_wider(no_dupls,
                     names_from = data.base,
                     values_from = n.casos) %>%
  data.frame()
data_wide[is.na(data_wide)] <- 0
data_wide
names(data_wide) <- str_remove(string = names(data_wide),pattern = "X")

data_wide <- tibble::column_to_rownames(.data = data_wide,
                                        var = "datainiciosintomas")

diff_se_maior <- function(x, y) {
  z <- if_else(y >= x,  y - x, 0)
  return(z)
  }

for (i in 1:(ncol(data_wide - 1))) {
  data_diff[,i] <- diff_se_maior(data_wide[,i], data_wide[,i + 1])
  }


dados_mun <- data_diff %>%
  rownames_to_column(var = "data_inicio") %>%
  pivot_longer(cols = -1, names_to = "data_base") %>%
  filter(value != 0) %>%
  mutate(data_inicio = lubridate::as_date(data_inicio),
         data_base = lubridate::as_date(data_base)) %>%
  data.frame()

window <- 40
dados.now <- NobBS::NobBS(data = dados_mun,
                          now = max(data_now$data_base), #- trim.now,
                          onset_date = "data_inicio",
                          report_date = "data_base",
                          units = "1 day",
                          moving_window = window)

now.dados.mun <- dados.now$estimates
now.dados.mun$estimate <- as.integer(now.dados.mun$estimate)
now.dados.mun$lower <- as.integer(now.dados.mun$lower)
now.dados.mun$upper <- as.integer(now.dados.mun$upper)


###agregando casos por data de sintoma############

casos_mun <- dados_mun %>%
  group_by(data_inicio) %>%
  summarise(Casos = n())  %>%
  as.data.frame()


#####juntando os casos estimados com reais#####

names(casos_mun) <- c("onset_date","estimate")
casos_mun_f <- casos_mun %>%
  filter(data_inicio < min(now.dados.mun$onset_date))

casos_mun_f$lower <- casos_mun_f$estimate
casos_mun_f$upper <- casos_mun_f$estimate

now.dados.mun <- now.dados.mun %>%
  select(estimate,onset_date, lower, upper)

casos.now.mun <- bind_rows(casos_mun_f, now.dados.mun)


###########fazendo a cumulativa####################
cum.casos.now.mun <- casos.now.mun
cum.casos.now.mun$estimate <- cumsum(casos.now.mun$estimate)
cum.casos.now.mun$lower <- cumsum(casos.now.mun$lower)
cum.casos.now.mun$upper <- cumsum(casos.now.mun$upper)


######cortando data para o gráfico###########
ggplot(casos.now.mun, aes(x = onset_date, y = estimate)) +
  geom_line(data = casos_mun, aes(x = onset_date, y = estimate), size = 1) +
  geom_line(data = casos.now.mun,
            aes(x = onset_date, y = estimate), size = 1, col = "red") +
  geom_ribbon(data = casos.now.mun, aes(ymin = lower, ymax = upper),
              fill = RColorBrewer::brewer.pal(3, "Set1")[2], alpha = 0.5) +
  scale_color_manual(name = "", values = RColorBrewer::brewer.pal(3, "Set1")[1:2]) +
  xlab("Data primeiro Sintoma") +
  ylab("Número novos casos leves/dia") +
  theme_minimal() +
  ggtitle("Casos leves de COVID em Florianópolis") +
  theme(legend.position = c(0.2, 0.8))
ggsave("./scripts_R_genericos/summary_esus/Florianopolis.png")
