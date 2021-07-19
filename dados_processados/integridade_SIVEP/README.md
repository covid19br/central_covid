# Integridade SIVEP

Uma forma de analisar a integridade dos dados é comparar o número de casos / óbitos de SRAG / Covid confirmados ao longo do tempo, para cada base publicada. Assim, se houver erros de leitura consideráveis, fica visível pelos gráficos que há menos casos computados do que o esperado a partir das bases anteriores.

# Diferença entre bases

O cálculo acima permite que se compare o número de eventos entre diferentes bases, o que se traduz numa data de notificação, permitindo seu uso para *nowcasting*.

# Formato dos arquivos

Os arquivos com nome começando com `age` são estruturados por idade em faixas de 10 em 10 anos, de 0-9, 10-19, ... 80+. Os arquivos com nome terminado em `br` são agregados para o Brasil todo, os terminados em `est` são agregados por estado. Os arquivos com `ob` no nome computam óbitos (os demais são hospitalizações), os arquivos começados com `age` computam os dados por faixa et

| campo      | formato  | conteúdo |
| ---------- | -------- | -------- |
| data       | %Y-%m-%h | data da base ao qual o dado se refere |
| dt_sin_pri | %Y-%m-%h | data de primeiro sintomas, que é a data de referência do caso. Esta coluna é a data de óbito nos arquivos pra óbitos |
| age_class  | age_N    | faixa etária, "age_1" = 0-9, "age2" = 10-19 etc, age_9 = 80+ |
| sg_uf      | XX       | sigla do estado |
| n          | int      | número de casos |
