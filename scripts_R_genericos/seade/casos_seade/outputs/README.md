# Outputs de análises de dados do SEADE - Plano SP

## Totais e máximos de casos e óbitos por semana de notificação ou sintoma
### Planilhas
* `totais_maximos_e_semana_pico_semana_notificacao.csv `: valores para casos agregados por semana da notificação
* `totais_maximos_e_semana_pico_semana_sintoma.csv` : valores para casos agregados por semana do primeiro sintoma

### Variáveis

* nome_drs : Nome da Delegacia Regional de Saúde (DRS)
* tot.casos: total de casos até a semana final que consta na planilha 	`casos_e_incidencias_DRS_semana_de_notificacao_e_sintomas.csv`
* tot.casos.pc	: taxa de ataque (casos/100mil hab) dos casos até a semana final que consta na planilha `casos_e_incidencias_DRS_semana_de_notificacao_e_sintomas.csv`
* tot.obitos	: total de óbitos  até a semana final que consta na planilha 	`casos_e_incidencias_DRS_semana_de_notificacao_e_sintomas.csv`
* tot.obitos.pc: mortalidade (óbitos/100mil hab) dos óbitos até a semana final que consta na planilha 	`casos_e_incidencias_DRS_semana_de_notificacao_e_sintomas.csv`
* casos.max: numero de casos na semana epiemdiológica em que houve o maior número de casos registrados
* casos.pc.max: taxa de ataque (casos/100.00 habitantes) na semana epiemdiológica em que houve o maior número de casos registrados
* semana.casos.max: semana epidemiológica em que houve maior número de casos resgistrados	
* obitos.max	: numero de óbitos na semana epiemdiológica em que houve o maior número de óbitos registrados
* obitos.pc.max	: mortalidade (óbitos/100.00 habitantes) na  na semana epiemdiológica em que houve o maior número de óbitos registrados
* semana.obitos.max: semana epidemiológica em que houve maior número de óbitos registrados

## Máximos e mínimos de dados de internação por DRS
### PLanilha
* `maximos_minimos_internacoes.csv`
### Variáveis
* nome_drs2: nome da DRS
* pacientes_uti_mm7d.min: mínimo da média móvel para 7 dias do Pacientes Internados em Leitos de UTI Destinados para COVID-19 no dia
* pacientes_uti_mm7d.max: máximo da média móvel para 7 dias do Pacientes Internados em Leitos de UTI Destinados para COVID-19 no dia
* total_covid_uti_mm7d.min: mínimo da média móvel para 7 dias do Total de Leitos de UTI Destinados para COVID-19 no dia
* total_covid_uti_mm7d.max: máximo da média móvel para 7 dias do Total de Leitos de UTI Destinados para COVID-19 no dia
* leitos_pc.min: mínimo de leitos Covid-19 UTI por 100 mil habitantes 
* leitos_pc.max: máximo de leitos Covid-19 UTI por 100 mil habitantes 
* ocupacao_leitos.min: mínimo da ocupação de leitos de UTI destinados para COVID-19 (pacientes_uti_mm7d / total_covid_uti_mm7d)
* ocupacao_leitos.max:máximo da ocupação de leitos de UTI destinados para COVID-19 (pacientes_uti_mm7d / total_covid_uti_mm7d)
* internacoes_7d.min: mínimo do número de novas internações (UTI e Enfermaria) de pacientes confirmados ou com suspeita de COVID-19 nos últimos 7 dias
* internacoes_7d.max: máximo do número de novas internações (UTI e Enfermaria) de pacientes confirmados ou com suspeita de COVID-19 nos últimos 7 dias
* internacoes_7d_pc.min: mínimo de `internacoes_7d` por 100 mil habitantes
* internacoes_7d_pc.max: máximo de `internacoes_7d` por 100 mil habitantes
* dia.pacientes_uti_mm7d.min: data de `pacientes_uti_mm7d.min` 
* dia.pacientes_uti_mm7d.max: data de `pacientes_uti_mm7d.max`
* dia.leitos_pc.min: data de `leitos_pc.min` 
* dia.leitos_pc.max: data de `leitos_pc.max`
* dia.ocupacao_leitos.min: data de `ocupacao_leitos.min`
* dia.ocupacao_leitos.max: data de `ocupacao_leitos.max`
* dia.internacoes_7d_pc.min: data de `internacoes_7d_pc.min`
* dia.internacoes_7d_pc.max: data de `internacoes_7d_pc.min`

## Números de casos e óbitos por semana epidemiológica e DRS
### Planilha
*  `casos_e_incidencias_DRS_semana_de_notificacao_e_sintomas.csv`

## Variáveis

* nome_drs: 	Nome da Delegacia Regional de Saúde (DRS)
* dt_semana: data do último dia da semana epidemiológica
* semana: semana epidemiológica
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
