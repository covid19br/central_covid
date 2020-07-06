#!/bin/bash

RUNFILE="nowcasting_robot_caller_DRS.run"
Rfolder="../nowcasting"

if [ -f $RUNFILE ]; then
    exit
fi

touch $RUNFILE

source functions.sh

estado="SP"
trim=5

echo Rodando ./auto_site_escala.sh municipio $estado $trim `seq 1 17`
./auto_site_escala.sh drs $estado $trim `seq 1 17`

for geocode in `seq 1 17`; do
    # atualiza plots
    latest=`get_latest '../dados_processados/nowcasting/DRS/SP/Grande_Sao_Paulo/tabelas_nowcasting_para_grafico/nowcasting_acumulado_covid_{data}.csv'`
    pushd $Rfolder
    Rscript update_nowcasting.R --escala drs --sigla $estado --geocode $geocode --dataBase $latest --outputDir ../dados_processados/nowcasting --trim $trim --updateGit TRUE --plot TRUE
    popd
done

latest=`get_latest '../dados_processados/nowcasting/DRS/SP/Grande_Sao_Paulo/tabelas_nowcasting_para_grafico/nowcasting_acumulado_covid_{data}.csv'`

## report
pushd ../dados_processados/nowcasting/DRS/SP/reports/
Rscript -e "rmarkdown::render(input = 'report.Rmd',
                              output_file = 'relatorio_${latest}.html',
                              output_dir = './')" &&
git add relatorio_${latest}.html Rt_DRS_max_${latest}.csv Rt_DRS_median_${latest}.csv &&
git commit -m ":robot: relatório DRS ${estado} de ${latest}" &&
git push
popd

rm $RUNFILE
