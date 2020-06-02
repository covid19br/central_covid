#!/bin/bash

# script assume que é rodado dentro do meta-repo, numa pasta abaixo da raiz
# ../nowcasting: repo nowcasting
# ../site: repo site

SCRIPTROOT=$PWD

estado="SP"
# SP, Araraquara, Jundiaí
#("355030" "350320" "352590")
# toma lista de municípos dos argumentos de entrada
municipios=( "$@" )
datafolder="../dados/estado_${estado}/SRAG_hospitalizados"
Rfolder="../nowcasting"
# convertendo caminhos relativos em absolutos
# realpath é mais profissa, mas não é garantido ter em todo lugar
if [ ${datafolder:0:1} = '/' ]; then
    absdatafolder=$datafolder
else
    absdatafolder="$PWD/$datafolder"
fi

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
out="../site/dados/projecao_leitos/municipios/${estado}/${nomes_municipios[${municipios[0]}]}/curve_fits/curve_fits_${todaydash}.Rds"
RUNFILE="modelogro_site_municipios_${estado}.run"

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

# esta é arcana...
fit_output_files="curve_fits_${todaydash}.Rds"
reports_files="${todaydash}_relatorio_projecoes_demanda_hospitalar_{srag,covid}.pdf"
hosp_output_files="hopitalized_UTI_${todaydash}.csv 
hopitalized_${todaydash}.csv"
web_output_files="last.update.modelogro.txt
plot.{covid,srag}.{leitos,uti}.forecast.{exp,logistic}.{,lg.,md.,sm.}svg
plot.{covid,srag}.{leitos,uti}.forecast.{exp,logistic}.html"
# eval expande todos wildcards nas variáveis
reports_files=`eval echo $reports_files`
web_output_files=`eval echo $web_output_files`

if [[ $newcommit && -f $csv2 && ! -f $out && ! -f $RUNFILE ]]; then
    touch $RUNFILE

    pushd $Rfolder
    for geocode in ${municipios[@]}; do
        ## nowcasting
        Rscript update_projecao_leitos.R --dir $absdatafolder/dados --sigla $estado --geocode $geocode --dataInicial "2020-03-08" --out_dir ../site/dados/ --check_report TRUE

        ## mandando pro site
        munpath="projecao_leitos/municipios/${estado}/${nomes_municipios[$geocode]}"
        
        # atualiza repo site
        # isto é feito a cada passo do loop - mais seguro?
        pushd $SCRIPTROOT/../site/_src
        Rscript update_modelogro.R --sigla $estado --geocode $geocode

        git pull --ff-only

        pushd ../dados/$munpath/hospitalizados
        git add $hosp_output_files
        popd
        
        pushd ../dados/$munpath/curve_fits
        git add $fit_output_files
        popd
        
        pushd ../dados/$munpath/reports
        git add $reports_files
        popd
        
        # pushd ../dados/$munpath/figures
        # git add $reports_files
        # popd
        
        pushd ../web/$munpath
        git add $web_output_files
        popd
        
        git commit -m ":robot: novas projeções de leitos e plots ${estado}-${nomes_municipios[$geocode]} ${today_}" &&
        git push
        popd
    done
    popd

    rm $RUNFILE
fi
