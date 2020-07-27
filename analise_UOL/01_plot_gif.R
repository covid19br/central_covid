###Análise UOL###
#################
source("./analise_UOL/load.R")
PRJROOT <- rprojroot::find_root(".here")

# sources functions
devtools::load_all("./now_fcts/") ##loading de funções necessárias##

# plot
data_ultimo_boletim <- as.Date("2020-07-21")
#ast: o ideal aqui seria usar get.last.date de uma pasta com tudo organizado
# e com relação aos dados seria ótimo ter um só arquivo para ler -
# uol<-read_csv("./analise_UOL/SRAGs-tabela-last-updated_revised-29_maio.csv") ### load da CSV, essa é sem preenchimento dos BE faltantes
uol <- readr::read_csv("./analise_UOL/dados/SRAGs-tabela-last-updated.csv")
###SEGUIR IGUAL###

uol <- as.data.frame(uol)
uol$Data <- as.Date(uol$Data, format = "%d/%m/%Y")
uol_melted <- reshape::melt(uol, id.vars = "Data")

p.uol <-
  ggplot(uol_melted, aes(x = Data, y = value, col = variable)) +
  geom_point(shape = 1) +
  geom_line() +
  geom_vline(xintercept = as.Date("2020-03-17", format = "%Y-%m-%d"),
             colour = "indianred3", size = 0.45, linetype = "dashed") +
  scale_color_viridis_d(name = "Data Boletim", option = "viridis", direction = 1) +
  labs(x = "Data", y = "Número de Óbitos") +
  theme_bw() +
  theme(legend.position = "right") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))
p.uol

p.uol.ridges <-
  ggplot(uol_melted, aes(x = Data, height = value, y = variable, fill = ..x..)) +
  ggridges::geom_density_ridges_gradient(scale = 3, stat = "identity", rel_min_height = 0.01) +
  ggridges::theme_ridges() +
  viridis::scale_fill_viridis(name = "Óbitos por dia", option = "viridis") +
  xlab("Data de óbito") +
  ylab("Boletins") +
  ggtitle("Boletins Epidemiológicos - Ministério da Saúde") +
  theme(legend.position = "none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8))
p.uol.ridges
