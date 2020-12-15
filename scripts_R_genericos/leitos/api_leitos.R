install.packages(c("httr", "jsonlite"))
install.packages("writexl")


#libraries
library(writexl)
library(httr)
library(jsonlite)


#Requisição API
#user: user-api-leitos
#password: aQbLL3ZStaTr38tj

url = "https://elastic-leitos.saude.gov.br/leito_ocupacao/_search"

res = GET(url, authenticate("user-api-leitos", "aQbLL3ZStaTr38tj"))

#Só consigo puxar com usuario e senha mas não consigo usar o query para nenhuma variavel
#res = GET(url, authenticate("user-api-leitos", "aQbLL3ZStaTr38tj", query = list(siglaEstado = "PR")))

res

res$url

#Converter conteúdo da resposta em texto
mycontenttext = httr:: content(res, as = 'text')
mycontenttext

#Converter conteúdo texto da resposta em json
mycontentjson = jsonlite::fromJSON(mycontenttext,  flatten = TRUE)
mycontentjson

#Converter json em dataframe
mycontentdf = as.data.frame(mycontentjson)
mycontentdf

#Salvar dataframe em excel
write_xlsx(x = mycontentdf, path = "teste.xlsx", col_names = TRUE)
GET
#Ele mostra apenas os 10 primeiros dados, ou seja, algo esta errado
