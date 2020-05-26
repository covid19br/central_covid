#!/bin/bash

# script assume que é rodado dentro do meta-repo, numa pasta abaixo da raiz
# ../nowcasting: repo nowcasting
# ../site: repo site

SCRIPTROOT=$PWD

estado="SP"
# SP, Araraquara, Jundiaí
municipios=("355030" "350320" "352590")
datafolder="../dados/estado_${estado}/SRAG_hospitalizados"
Rfolder="../nowcasting"
if [ ${datafolder:0:1} = '/' ]; then
    absdatafolder=$datafolder
else
    absdatafolder="$PWD/$datafolder"
fi

# TODO: discutir tamanho do trim
trim=2

today=`LANG=en date +'%b %-d'`
today_=`date +'%Y_%m_%d'`
todaydash=`date +'%Y-%m-%d'`

# associative array!
declare -A nomes_municipios
for mun in ${municipios[@]}; do
    nomes_municipios[$mun]=`awk -F, '/'"$mun"'/ {gsub(/"/, "", $13); print $13}' ../nowcasting/dados/geocode_ibge.csv`
done

# csv: só usa já processado (depende do trabalho do auto_DRS_nowcast_report.sh)
csv2="$absdatafolder/dados/SRAGH_${today_}.csv"
out="../site/web/${estado}/${nomes_municipios[${municipios[0]}]}/last.update.txt"
RUNFILE="nowcasting_site_municipios_${estado}.run"

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

if [[ $newcommit && -f $csv2 && ! -f $out && ! -f $RUNFILE ]]; then
    touch $RUNFILE

    pushd $Rfolder
    for geocode in ${municipios[@]}; do
        ## nowcasting
        Rscript update_nowcasting.R --dir $absdatafolder/dados --escala municipio --geocode $geocode --dataBase $today_ --outputDir $absdatafolder/outputs --trim $trim --updateGit TRUE

        ## mandando pro site
        munpath="municipios/${estado}/${nomes_municipios[$geocode]}"
        cp -r $absdatafolder/outputs/$munpath/tabelas_nowcasting_para_grafico/*$today_.csv $SCRIPTROOT/../site/dados/$munpath/tabelas_nowcasting_para_grafico/
        pushd $SCRIPTROOT/../site/_src
        Rscript update_plots_nowcasting.R --escala municipio --geocode $geocode --dataBase $today_
        cd ..
        ### DANGER DANGER DANGER ###
        git add dados/$munpath/tabelas_nowcasting_para_grafico/*$today_.csv web/$munpath &&
        git commit -m ":robot: outputs nowcasting ${estado}-${nomes_municipios[$geocode]} ${today_}"
        popd
    done
    popd

    rm $RUNFILE
fi
