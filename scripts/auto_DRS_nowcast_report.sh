#!/bin/bash

# script assume que é rodado dentro do meta-repo, numa pasta abaixo da raiz
# ../nowcasting: repo nowcasting

estado="SP"
nDRS=`seq 1 17`
datafolder="../dados/estado_${estado}/SRAG_hospitalizados"
Rfolder="../nowcasting"
if [ ${datafolder:0:1} = '/' ]; then
    absdatafolder=$datafolder
else
    absdatafolder="$PWD/$datafolder"
fi

# TODO: discutir tamanho do trim
trim=5

today=`LANG=en date +'%b %-d'`
today_=`date +'%Y_%m_%d'`
todaydash=`date +'%Y-%m-%d'`

# csv pode já ter sido processado
csv="$absdatafolder/dados/Pacientes_internados_com_SRAG_data${todaydash}.csv"
csv2="$absdatafolder/dados/SRAGH_${today_}.csv"
out="$absdatafolder/reports/relatorio_${today_}.html"
RUNFILE="nowcasting_DRS_${estado}.run"

# pull do meta-repo: *DANGER HERE*
# este pull é pra você poder atualizar o meta-repo depois - se tiver base nova
# o commit do submodulo dele estará *desatualizado*
git pull --recurse-submodules --ff-only
pushd $absdatafolder
# AQUI pegamos alterações novas, sem detached HEAD no submodule
git checkout master && git pull --ff-only &&
git log -- dados/ | grep  "$today"
newcommit=$?
popd

if [[ $newcommit && ( -f $csv || -f $csv2 ) && ! -f $out && ! -f $RUNFILE ]]; then
    touch $RUNFILE

    ## process file
    if [[ ! -f $csv2 ]]; then
        pushd $absdatafolder/dados
        git mv "Pacientes_internados_com_SRAG_data${todaydash}.csv" "SRAGH_${today_}.csv" &&
        # troca espaço por _ no cabeçalho
        sed -i '1 s/ /_/g' "SRAGH_${today_}.csv" &&
        git add "SRAGH_${today_}.csv" &&
        git commit -m ":robot: corrigindo nome e cabeçalho SIVEP estado ${estado}" &&
        git push
        popd
        # commit no meta-repo ## DANGER HERE ##
        # TODO: path não é geral, tem solução geral?!
        # IMPORTANTE: isto é feito aqui (mesmo sendo repetido no fim do script)
        # por precaução, porque os scripts abaixo podem (em tese, se eles forem
        # alterados) dar pull no meta-repo e puxar o commit antigo do meta-repo
        git commit ../dados/estado_$estado -m ":robot: Atualizando commit estado ${estado}" &&
        git push
    fi

    ## nowcasting
    pushd $Rfolder
    for DRS in $nDRS; do
        Rscript update_nowcasting.R --dir $absdatafolder/dados --escala drs --sigla $estado --geocode $DRS --dataBase $today_ --outputDir $absdatafolder/outputs --trim $trim --updateGit TRUE
        Rscript update_nowcasting.R --dir $absdatafolder/dados --escala drs --sigla $estado --geocode $DRS --dataBase $today_ --outputDir $absdatafolder/outputs --trim $trim --updateGit TRUE --plot TRUE
        # TODO: add projecao leitos
        # Rscript update_projecao_leitos.R --dir $absdatafolder/dados/ --escala drs --sigla $estado --geocode $DRS --dataBase $today_ --dataInicial 2020-03-16 --out_dir $absdatafolder/outputs/
    done
    popd

    ## report
    pushd $absdatafolder/reports
    Rscript -e "rmarkdown::render(input = 'report.Rmd',
                                  output_file = 'relatorio_${today_}.html',
                                  output_dir = './')"
    git add "relatorio_${today_}.html" &&
    git commit -m ":robot: relatório DRS ${estado} de hoje" &&
    git push
    popd

    # atualizando meta-repo (de novo)
    git commit ../dados/estado_$estado -m ":robot: Atualizando commit estado ${estado}" &&
    git push

    rm $RUNFILE
fi
