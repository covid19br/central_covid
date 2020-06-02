#!/bin/bash

# script assume que é rodado dentro do meta-repo, numa pasta abaixo da raiz
# ../nowcasting: repo nowcasting
# ../site: repo site

SCRIPTROOT=$PWD

# SP, Araraquara, Jundiaí
#("355030" "350320" "352590")
# toma lista de municípos dos argumentos de entrada
datafolder="../dados/municipio_campinas/SRAG_hospitalizados"
Rfolder="../nowcasting"
outfolder="../dados/municipio_campinas/output/"
# convertendo caminhos relativos em absolutos
# realpath é mais profissa, mas não é garantido ter em todo lugar
if [ ${datafolder:0:1} = '/' ]; then
    absdatafolder=$datafolder
else
    absdatafolder="$PWD/$datafolder"
fi
if [ ${outfolder:0:1} = '/' ]; then
    absoutfolder=$absoutfolder
else
    absoutfolder="$PWD/$absoutfolder"
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
csv="$absdatafolder/dados/SIVEP_CPS_${today_}.csv"
out="../dados/municipio_campinas/output/projecao_leitos/municipios/SP/Campinas/relatorios/{todaydash}_relatorio_projecoes_demanda_hospitalar_srag.pdf"
RUNFILE="modelogro_municipios_campinas.run"

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
# eval expande todos wildcards nas variáveis
reports_files=`eval echo $reports_files`

if [[ $newcommit && -f $csv && ! -f $out && ! -f $RUNFILE ]]; then
    touch $RUNFILE

    pushd $Rfolder
    ## nowcasting
    # ATENÇÃO: se UPDATE_GIT_DATA_REPO for FALSE dados *não são* salvos,
    # permanecem como cópia local, suja. Se deseja limpar, pode rodar
    # depois:
    # cd $absoutfolder/outputs; git clean -f
    # que *apaga* todos arquivos untracked (DANGER)
    Rscript update_projecao_leitos.R --dir $absdatafolder/dados --geocode 3509502 --dataInicial "2020-03-16" --out_dir $outfolder --check_report TRUE
    popd

    ## mandando pro repo de campinas

    pushd $outfolder
    git pull --ff-only

    munpath="./projecao_leitos/municipios/SP/Campinas"

    pushd $munpath/hospitalizados
    git add $hosp_output_files
    popd
    
    pushd $munpath/curve_fits
    git add $fit_output_files
    popd
    
    pushd $munpath/reports
    git add $reports_files
    popd

    git commit -m ":robot: novas projeções de leitos e relatórios Município Campinas ${today_}" &&
    git push
    popd

    rm $RUNFILE
fi
