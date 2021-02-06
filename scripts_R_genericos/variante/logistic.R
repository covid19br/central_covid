library(ggplot2)
library(viridis)

# dados obtidos por inspeção visual! 2021-02-05
# http://www.genomahcov.fiocruz.br/presenca-das-linhagens-por-estado/
d = data.frame(t=11:13,
               P1 = c(0, 46, 39),
               outras = c(24, 50, 3))
d$freq = d$P1 / (d$P1 + d$outras)
model = glm(cbind(P1, outras) ~ t, data = d, family = "binomial")
#summary(model)

# dados da nota técnica
# http://www.genomahcov.fiocruz.br/wp-content/uploads/2021/02/NOTA-TEECNICA-CONJUNTA-No-09.2021.FVS-AM-X-ILMD.FICRUZ-AM-28.01.2021-.pdf
d2 = data.frame(t=11:13,
               P1 = c(0, 28, 32),
               outras = c(24, 27, 3))
d2$freq = d2$P1 / (d2$P1 + d2$outras)
model2 = glm(cbind(P1, outras) ~ t, data = d2, family = "binomial")

p <- ggplot(d) +
    geom_point(aes(x=t, y=freq)) +
    stat_function(fun = function(t) 1/(1+exp(-model$coefficients[2]*t - model$coefficients[1]))) +
    #geom_point(data=d2, aes(x=t, y=freq)) +
    #stat_function(fun = function(t) 1/(1+exp(-model2$coefficients[2]*t - model2$coefficients[1]))) +
    ylim(0, 1) +
    labs(title = "logistic fit", x = "month", y = "frequency of P.1 variant") +
    annotate("label", x = 12.5, y = 0.15, label =
             paste("Diferença entre Reff's das variantes:", round(13 * model$coefficients[2] / 30, 1),
             "\n [", paste(round(13 * confint(model, level=0.68)["t",] / 30, 1), collapse = "; "), "] (68% conf. int.)",
             "\n [", paste(round(13 * confint(model, level=0.95)["t",] / 30, 1), collapse = "; "), "] (95% conf. int.)"))

#lines(tt, 1/(1+exp(-model$coefficients[2]*tt - model$coefficients[1])))

print("Diferença de Reff's entre variantes:")
print(13 * confint(model, level=0.8)["t",] / 30)


## calculo prob. de reinfecção pela 2a variante

prob_reinfec <- function(prevalencia, inc_Reff){
    S = 1 - prevalencia
    (S * inc_Reff - S) / prevalencia
}

ii = seq(0.3, 1., by = 0.1)
x <- seq(0.1, 0.95, by = 0.01)
fun.data <- list()
for (i in ii)
    fun.data[[paste0(100 * i, " %")]] <- data.frame(x=x, y=prob_reinfec(x, 1+i))
fun.data <- plyr::ldply(fun.data, .id="incR")
p_reinf <- ggplot(fun.data) +
    geom_line(aes(x=x, y=y, col=incR)) +
    xlim(0.1, 0.95) +
    ylim(0, 1.) +
    scale_color_manual(name = "Increase in Reff",
                       values = viridis(length(ii)), # Color specification
                       labels = paste(100 * ii, "%")) +
    labs(x = "prevalence (December)", y = "probability of reinfection",
         title = "Probability of reinfection by new variant")


## "média ponderada" das taxas de crescimento

Reff.pond <- function(gama, freq.P1, R.orig, R.P1, R.window) {
    1 + 1/gama * log((1-freq.P1) * exp(gama * (R.orig - 1) * R.window) + freq.P1 * exp(gama * (R.P1-1) * R.window)) / R.window
}

Reff.pond(gama = 30 / 13,
          freq.P1 = 0.5,
          R.orig = 0.9,
          R.P1 = 2.0,
          R.window = 7/30)


