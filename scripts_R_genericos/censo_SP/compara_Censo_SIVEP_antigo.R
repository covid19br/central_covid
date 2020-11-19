# script assume locais e nomes do arquivo do Censo *fora* dos repos
library(lubridate)
library(dplyr)
library(ggplot2)
if(!require(cowplot)){install.packages("cowplot"); library(cowplot)}

outdir_modelogro <- "../../dados/estado_SP/SRAG_hospitalizados/outputs/projecao_leitos/DRS/SP"
dir_censo <- "../../../Censo_Covid/"

data.base <- "2020_05_28"
data.proj.leitos="2020-06-06"

## Censo COVID

# to clean dates in raw data
# d <- read.csv(paste0("dados/CensoCOVID_data_", data.base, ".csv"), sep=';')
# d$Data.da.Notificacao <- zoo::as.Date.numeric(sapply(d$Data.da.Notificacao, function(x) as_date(parse_date_time(x, c("dmy", "ydm HMs"))), USE.NAMES=F))
# write.csv(d, paste0("CensoCOVID_", data.base, ".csv"), row.names=F)

d <- read.csv(paste0(dir_censo, "CensoCOVID_", data.base, ".csv"))
d.DRS <- d %>% 
    group_by(DRS.Numero, Data.da.Notificacao) %>%
    summarise(DRS.Nome = first(DRS.Nome),
              registros = n(),
              confirmado.uti = sum(Pacientes.UTI...Confirmados),
              suspeito.uti = sum(Pacientes.UTI...Suspeitos),
              confirmado.enf = sum(Pacientes.Enfermaria.ou.Retaguarda.PS...Confirmados),
              suspeito.enf = sum(Pacientes.Enfermaria.ou.Retaguarda.PS...Suspeitos),
              leitos.uti = sum(Quantidade.de.leitos.de.UTI.declarados),
              leitos.enf = sum(Quantidade.de.leitos.de.Enfermaria.declarados),
              leitos.covid.uti = sum(Total.de.Leitos.Destinados.para.COVID.19...UTI),
              leitos.covid.enf = sum(Total.de.Leitos.Destinados.para.COVID.19...Enfermaria),
              leitos.disp.uti = sum(Total.de.Leitos.Disponiveis...UTI),
              leitos.disp.enf = sum(Total.de.Leitos.Disponiveis...Enfermaria)
    ) %>%
    arrange(DRS.Numero, Data.da.Notificacao) %>%
    as.data.frame()

## output projeção leitos
d.sivep <- list()
for (drs in unique(d.DRS$DRS.Nome)){
    drs.ascii <- gsub("'", "", gsub(" ", "_", textclean::replace_non_ascii(drs)))
   
    hosp <- read.csv(paste0(outdir_modelogro, "/", drs.ascii, "/hospitalizados/hopitalized_", data.proj.leitos, ".csv"))
    colnames(hosp)[-c(1,6)] <- paste0(colnames(hosp)[-c(1,6)], ".hosp")
    uti <- read.csv(paste0(outdir_modelogro, "/", drs.ascii, "/hospitalizados/hopitalized_UTI_", data.proj.leitos, ".csv"))
    colnames(uti)[-c(1,6)] <- paste0(colnames(uti)[-c(1,6)], ".uti")
    d.sivep[[drs]] <- merge(uti, hosp)
}
dados.sivep <- bind_rows(d.sivep, .id = "DRS.Nome")

# WARNING: extreme POG ahead?
data <- merge(d.DRS, dados.sivep[dados.sivep$type=='covid',], by.x = c("DRS.Nome", "Data.da.Notificacao"), by.y = c("DRS.Nome", "date"))
data <- merge(data, dados.sivep[dados.sivep$type=='srag',], by.x = c("DRS.Nome", "Data.da.Notificacao"), by.y = c("DRS.Nome", "date"), suffixes=c('', '.srag'))

data$Data.da.Notificacao <- as.Date(data$Data.da.Notificacao)

### plots com facets por DRS
# confirmado UTI
plot.uti.confirmado <- ggplot(data = data, aes(x=Data.da.Notificacao)) +
    geom_point(aes(y = confirmado.uti, color="Censo-COVID")) +
    geom_point(aes(y = observed.uti, color="SIVEP observado")) +
    geom_line(aes(y = estimate.uti, color = "SIVEP nowcast")) +
    #geom_ribbon(fill="indianred3", 
    #            aes(ymin=lower.uti, ymax=upper.uti), alpha=0.2, color = 0) +
    theme_cowplot() +
    xlim(start=as.Date("2020-04-01"), end=as.Date("2020-05-26")) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") +
    background_grid(major = "xy", minor = "y") +
    facet_wrap(~ DRS.Nome, scales="free") +
    ggtitle("Covid confirmado UTI") +
    theme(legend.position = c(0.7, 0), legend.justification = c(1, 0),
          legend.text=element_text(size=16)) +
    scale_colour_manual(values = c('Censo-COVID' = 'black',
                                   'SIVEP observado' = 'darkgrey',
                                   'SIVEP nowcast' = 'indianred3'))
ggsave(plot = plot.uti.confirmado, file = "uti_confirmado.png", width=15, height=10)

# confirmado Enfermaria
plot.enf.confirmado <- ggplot(data = data, aes(x=Data.da.Notificacao)) +
    geom_point(aes(y = confirmado.enf, color="Censo-COVID")) +
    geom_point(aes(y = observed.hosp - observed.uti, color="SIVEP observado")) +
    geom_line(aes(y = estimate.hosp - estimate.uti, color = "SIVEP nowcast")) +
#    geom_ribbon(fill="indianred3", 
#                aes(ymin=lower.uti, ymax=upper.uti), alpha=0.2, color = 0) +
    theme_cowplot() +
    xlim(start=as.Date("2020-04-01"), end=as.Date("2020-05-26")) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") +
    background_grid(major = "xy", minor = "y") +
    facet_wrap(~ DRS.Nome, scales="free") +
    ggtitle("Covid confirmado Enfermaria") +
    theme(legend.position = c(0.7, 0), legend.justification = c(1, 0),
          legend.text=element_text(size=16)) +
    scale_colour_manual(values = c('Censo-COVID' = 'black',
                                   'SIVEP observado' = 'darkgrey',
                                   'SIVEP nowcast' = 'indianred3'))
ggsave(plot = plot.enf.confirmado, file = "enf_confirmado.png", width=15, height=10)

# suspeito UTI
plot.uti.suspeito <- ggplot(data = data, aes(x=Data.da.Notificacao)) +
    geom_point(aes(y = suspeito.uti, color="Censo-COVID")) +
    geom_point(aes(y = observed.uti.srag, color="SIVEP observado")) +
    geom_line(aes(y = estimate.uti.srag, color = "SIVEP nowcast")) +
    #geom_ribbon(fill="indianred3", 
    #            aes(ymin=lower.uti.srag, ymax=upper.uti.srag), alpha=0.2, color = 0) +
    theme_cowplot() +
    xlim(start=as.Date("2020-04-01"), end=as.Date("2020-05-26")) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") +
    background_grid(major = "xy", minor = "y") +
    facet_wrap(~ DRS.Nome, scales="free") +
    ggtitle("Covid suspeito/SRAG UTI") +
    theme(legend.position = c(0.7, 0), legend.justification = c(1, 0),
          legend.text=element_text(size=16)) +
    scale_colour_manual(values = c('Censo-COVID' = 'black',
                                   'SIVEP observado' = 'darkgrey',
                                   'SIVEP nowcast' = 'indianred3'))
ggsave(plot = plot.uti.suspeito, file = "uti_suspeito.png", width=15, height=10)

# suspeito Enfermaria
plot.enf.suspeito <- ggplot(data = data, aes(x=Data.da.Notificacao)) +
    geom_point(aes(y = suspeito.enf, color="Censo-COVID")) +
    geom_point(aes(y = observed.hosp.srag - observed.uti.srag, color="SIVEP observado")) +
    geom_line(aes(y = estimate.hosp.srag - estimate.uti.srag, color = "SIVEP nowcast")) +
#    geom_ribbon(fill="indianred3", 
#                aes(ymin=lower.uti, ymax=upper.uti), alpha=0.2, color = 0) +
    theme_cowplot() +
    xlim(start=as.Date("2020-04-01"), end=as.Date("2020-05-26")) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") +
    background_grid(major = "xy", minor = "y") +
    facet_wrap(~ DRS.Nome, scales="free") +
    ggtitle("Covid suspeito/SRAG Enfermaria") +
    theme(legend.position = c(0.7, 0), legend.justification = c(1, 0),
          legend.text=element_text(size=16)) +
    scale_colour_manual(values = c('Censo-COVID' = 'black',
                                   'SIVEP observado' = 'darkgrey',
                                   'SIVEP nowcast' = 'indianred3'))
ggsave(plot = plot.enf.suspeito, file = "enf_suspeito.png", width=15, height=10)

