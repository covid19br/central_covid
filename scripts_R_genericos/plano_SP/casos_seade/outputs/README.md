# Outputs de análises de dados do SEADE - Plano SP

## Dados das semanas de máximos de casos ou óbitos
### Planilhas
* `totais_maximos_e_semana_pico_semana_notificacao.csv `: valores para casos agregados por semana da notificação
* `totais_maximos_e_semana_pico_semana_sintoma.csv` : valores para casos agregados por semana do primeiro sintoma

### Variáveis

* nome_drs : Nome da Delegacia Regional de Saúde (DRS)
*  casos.max: numero de casos na semana epiemdiológica em que houve o maior número de casos registrados
* casos.pc.max: taxa de ataque (casos/100.00 habitantes) na  na semana epiemdiológica em que houve o maior número de casos registrados
* semana.casos.max: semana epidemiológica em que houve maior número de casos resgistrados	
* obitos.max	: numero de óbitos na semana epiemdiológica em que houve o maior número de óbitos registrados
* obitos.pc.max	: mortalidade (óbitos/100.00 habitantes) na  na semana epiemdiológica em que houve o maior número de óbitos registrados
* semana.obitos.max: semana epidemiológica em que houve maior número de óbitos registrados

## Planilha com números de casos e óbitos por semana epidemiológica e DRS
### Planilha
*  `casos_e_incidencias_DRS_semana_de_notificacao_e_sintomas.csv`

## Variáveis

* nome_drs: 	Nome da Delegacia Regional de Saúde (DRS)
* semana_epidem	: semana epidemiológica
* cod_drs.x	: código numeŕico da DRS (SEADE)
* pop	: população estimada da DRS (SEADE)
* pop_60	: população acima de 60 anos estimada (SEADE)
* area	:  soma das áreas dos municípios da DRS
* casos_not	: número de casos notificados na semana
* casos_sin	:  número de casos com data de primeiro sintoma na semana
* casos_not_pc	: taxa de ataque dos casos notificados na semana (casos/100mil hab) 
* casos_sin_pc	: taxa de ataque dos casos com primeiro sintoma na semana (casos/100mil hab) 
* obitos_not	: óbitos notificados na semana
* obitos_sin	: óbitos com data de primeiro sintoma na semana
* obitos_not_pc	:  mortalidade de óbitos notificados na semana (óbitos/100mil hab) 
* obitos_sin_pc: mortalidade de óbitos com primeiros sintomas na semana (óbitos/100mil hab) 
