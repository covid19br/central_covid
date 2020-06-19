# Análises de nowcasting de óbitos nacional

Essas análises são feitas e partir de raspagem dos gráficos publicados nos boletins epidemiológicos do Ministério da Saúde. Esse gráfico é o número de óbitos pela data de óbito, ou seja, cada óbito ali contabilizado ocorreu realmente naquela data. Com a falta de atualização da SIVEP-Gripe nacional, esse é um modo de estimar os óbitos no momento atual.

     .
    ├── dados
          ├── spreadsheet_e_CSV                  # CSVs gerados e usados para a análise
          └── boletins_epidemiologicos_MS/       # Pasta contendo o material 
                                        original do qual são raspados os dados para análise
    ├── codes/                             # Pasta com código em desuso ou ainda não terminados
    ├── plots/                             # Plots gerados
                                             nowcasting para os boletins compilados.
    ├── nowcasting_uol.R                   # Código da análise principal, roda 
    ├── analise_de_sensibilidade.R         # Código para comparar a performance do nowcasting
    └── README.md

Os scripts principais são `nowcasting_uol.R` e `analise_de_sensibilidade.R`. 

Dúvdas e sugestões:

- para tudo que for datado deveríamos usar o padrão YYYY_mm_dd para facilitar a organização
    R: Sim, mas as datas dos boletins precisam serem identificadas como char, pois são nomes de colunas originalmente.
- .csv na raiz de `analise_uol` deveriam ir para uma subpasta específica
    R: Concordo, só mudar os paths, mas separar o que está sendo usado.
- `boletins_epidemiologicos_MS` tem nomes originais dos arquivos disponibilizados pelo MS? Poderíamos retirar arquivos como (Campanha doação de leite?) e deixar apenas o essencial?
    R: Sim, só tomarmos cuidados que existem arquivos que não são boletins epidemiológicos que contém o gráfico que raspamos.
- arquivos em `codes/` são usados para que? poderiam ser numerados? `monta_matriz.R` e `monta_matriz.R (1)` tem diferença?
    R: São tanto testes como versões antigas do código atual, a numeração são cópias somente, perdão pela bagunça :(
- `pdfs` são relatorios internos gerados a partir de que?
    R: São relatórios internos e de conversas com o UOl, já retirados, não essenciais para ir pro site e análise.
- `spreadsheet_e_CSV` aqui tem arquivos de entrada e outputs? seria bom organizar em subpastas outputs que vem de um mesmo código e talvez arquivos de entrada em outra pasta
    R: Farei isso jajá
