# Análises de nowcasting de óbitos nacional

Essas análises são feitas e partir de raspagem dos gráficos publicaos nos boletins epidemiológicos do Ministério da Saúde.

     .
    ├── boletins_epidemiologicos_MS/       # 
    ├── codes/                             # 
    ├── pdfs/                              # 
    ├── plots/                             # 
    ├── spreadsheet_e_CSV                  # 
    ├── analise_de_sensibilidade.R         # 
    ├── nowcasting_uol.R                   # 
    └── README.md

Os scripts principais são `nowcasting_uol.R` e `analise_de_sensibilidade.R`. 

Dúvdas e sugestões:

- para tudo que for datado deveríamos usar o padrão YYYY_mm_dd para facilitar a organização
- .csv na raiz de `analise_uol` deveriam ir para uma subpasta específica
- `boletins_epidemiologicos_MS` tem nomes originais dos arquivos disponibilizados pelo MS? Poderíamos retirar arquivos como (Campanha doação de leite?) e deixar apenas o essencial?
- arquivos em `codes/` são usados para que? poderiam ser numerados? `monta_matriz.R` e `monta_matriz.R (1)` tem diferença?
- `pdfs` são relatorios internos gerados a partir de que?
- `spreadsheet_e_CSV` aqui tem arquivos de entrada e outputs? seria bom organizar em subpastas outputs que vem de um mesmo código e talvez arquivos de entrada em outra pasta
