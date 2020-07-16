## Exploracao inicial de prevalencias calculadas por alguns inqueritos

library(epiR)

## Terceira etapa do inquerito de Pelotas
## Sampa: 250 testes, 3 positivos
## Apresentacao do projeto indica sensiblidade de 77% e especifiidade de 98% (https://estado.rs.gov.br/upload/arquivos//covid-19-estudo-versao-atualizada.pdf)
## Inquerito da PM SP indica que o inquerito de Pelotas tem 66% de sensiblidade
epi.prev(3, 250, 0.77, 0.98) ## prevalencia aparente menor que 1- especificidade
epi.prev(3, 250, 0.66, 0.98)

## Imperatriz
epi.prev(31, 250, 0.77, 0.98) ## prevalencia aparente menor que 1- especificidade
epi.prev(31, 250, 0.66, 0.98)
