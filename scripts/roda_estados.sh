#!/bin/bash

RUNFILE="nowcasting_robot_caller_estados.run"
Rfolder="../nowcasting"
trim=2

if [ -f $RUNFILE ]; then
    exit
fi

touch $RUNFILE

source functions.sh

estados_id=("12" "27" "13" "16" "29" "23" "53" "32" "52" "21" "31" "50" "51" "15" "25" "26" "22" "41" "33" "24" "11" "14" "43" "42" "28" "35" "17")
estados_sigla=("AC" "AL" "AM" "AP" "BA" "CE" "DF" "ES" "GO" "MA" "MG" "MS" "MT" "PA" "PB" "PE" "PI" "PR" "RJ" "RN" "RO" "RR" "RS" "SC" "SE" "SP" "TO")

pushd $Rfolder
for i in "${!estados_id[@]}"; do
    echo "Rodando estado ${estados_id[$i]} ${estados_sigla[$i]}"
    Rscript update_nowcasting.R --dir ../dados/SIVEP-Gripe/ --escala estado --sigla ${estados_sigla[$i]} --geocode ${estados_id[$i]} --outputDir ../dados_processados/nowcasting/ --trim $trim --updateGit TRUE --Rmethod Cori
    Rscript update_nowcasting.R --dir ../dados/SIVEP-Gripe/ --escala estado --sigla ${estados_sigla[$i]} --geocode ${estados_id[$i]} --outputDir ../dados_processados/nowcasting/ --trim $trim --updateGit TRUE --Rmethod Cori --plot TRUE
done
popd

latest=`get_latest '../dados_processados/nowcasting/estado/AC/tabelas_nowcasting_para_grafico/nowcasting_acumulado_covid_{data}.csv'`

## report
pushd ../dados_processados/nowcasting/estado/reports/
Rscript -e "rmarkdown::render(input = 'report.Rmd',
                              output_file = 'relatorio_${latest}.html',
                              output_dir = './')"
git add "relatorio_${latest}.html" &&
git commit -m ":robot: relatório Brasil por estado de ${latest}" &&
git push
popd

rm $RUNFILE
