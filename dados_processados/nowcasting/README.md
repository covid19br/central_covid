# Planilhas resultantes das análises de nowcasting e R efetivo

## Estrutura dos diretórios

 * `estado/`: um subdiretório para cada estado do Brasil
 * `DRS/`: Delegacias Regionais de Saúde ou divisão equivalente para estados selecionados. Organizado como `[sigla do estado]/[nome da DRS]`
 * `municipios/`: municípios selecionados de todo Brasil. Organizado como `[sigla do estado]/[nome do município]`

Ao fim de cada caminho indicado acima haverá três  diretórios:

 * `output_nowcasting` : planilhas com os resultados da execução do script [update_nowcasting.R](https://github.com/covid19br/nowcasting/tree/master/_src), do repositório https://github.com/covid19br/nowcasting/. 
 * `tabelas_nowcasting_para_grafico`: planilhas para gerar os gráficos do site, geradas pelo mesmo script. 
 * `plots` : imagens dos gráficos e planilhas com alguns valores que são embebidos no texto do site. Resulta da execução do mesmo script, com a opção `--plot TRUE`.

## Planilhas que vão para o site

Por favor veja o [README](https://github.com/covid19br/covid19br.github.io/blob/master/dados/README.md) no repositório do site.
