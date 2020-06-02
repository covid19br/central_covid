#!/bin/bash

# script assume que é rodado dentro do meta-repo, numa pasta abaixo da raiz
# ../nowcasting: repo nowcasting
# ../site: repo site

usage(){
    echo -e "
USO: $0 [escala] [trim] {geocodes}

  escala: municipio | drs
  trim: dias de trim do nowcasting, geralmente 2-5
  geocodes: lista de geocodes de municípios ou ids de DRSs
"
}

if [ ${#@} -lt 3 ]; then
    usage
    exit 1
fi

SCRIPTROOT=$PWD

estado="SP"

escala=$1
shift
# TODO: discutir tamanho do trim (atual: 2 pra municipio, 5 pra DRS?)
trim=$2
shift
geocodes=( "$@" )

datafolder="../dados/estado_${estado}/SRAG_hospitalizados"
Rfolder="../nowcasting"
outfolder="../dados_processados/nowcasting"
# convertendo caminhos relativos em absolutos
# realpath é mais profissa, mas não é garantido ter em todo lugar
if [ ${datafolder:0:1} = '/' ]; then
    absdatafolder=$datafolder
else
    absdatafolder="$PWD/$datafolder"
fi
if [ ${outfolder:0:1} = '/' ]; then
    absoutfolder=$outfolder
else
    absoutfolder="$PWD/$outfolder"
fi


# atualiza repo onde dados estão?
UPDATE_GIT_DATA_REPO=TRUE

today=`LANG=en date +'%b %-d'`
today_=`date +'%Y_%m_%d'`
todaydash=`date +'%Y-%m-%d'`
today="May 31"
today_="2020_05_31"
todaydash="2020-05-31"

if [ $escala == "municipio" ]; then
    folder="municipios"
elif [ $escala == "drs" ]; then
    folder="DRS"
else
    usage
    exit 1
fi

# associative array of names
declare -A nomes
for geocode in ${geocodes[@]}; do
    if [ $escala == "municipio" ]; then
        nomes[$geocode]=`awk -F, '/'"$geocode"'/ {gsub(/"/, "", $13); print $13}' ../nowcasting/dados/geocode_ibge.csv`
    elif [ $escala == "drs" ]; then
        nomes[$geocode]=`awk -F, '{ if($2 == '"$geocode"') {gsub(/"/, "", $5); print $5}}' ../nowcasting/dados/DRS_SP.csv | head -n1`
    fi
done


# csv: só usa já processado (depende do trabalho do auto_DRS_nowcast_report.sh)
csv2="$absdatafolder/dados/SRAGH_${today_}.csv"
out="../site/dados/${folder}/${estado}/${nomes[${geocodes[0]}]}/tabelas_nowcasting_para_grafico/nowcasting_acumulado_covid_${today_}.csv"
RUNFILE="nowcasting_site_${folder}_${estado}.run"

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
output_files="nowcasting_{acumulado,diario}_{,obitos_}{covid,srag}_${today_}.csv
tempo_duplicacao_{,obitos_}{covid,srag}_${today_}.csv
r_efetivo_{covid,srag}_${today_}.csv"
web_output_files="last.update.txt
data_{forecast_exp,tempo_dupli}_{,obitos_}{covid,srag}.csv
data_Re_{covid,srag}.csv
plot_nowcast_{,cum_}{,ob_}{covid,srag}.html
plot_nowcast_{,cum_}{,ob_}{covid,srag}{,.ex,.lg,.md,.sm}.svg
plot_tempo_dupl_{,ob_}{covid,srag}.html
plot_tempo_dupl_{,ob_}{covid,srag}{,.ex,.lg,.md,.sm}.svg
plot_estimate_R0_{covid,srag}.html
plot_estimate_R0_{covid,srag}{,.ex,.lg,.md,.sm}.svg"
# eval expande todos wildcards nas variáveis
output_files=`eval echo $output_files`
web_output_files=`eval echo $web_output_files`

if [[ $newcommit && -f $csv2 && ! -f $out && ! -f $RUNFILE ]]; then
    touch $RUNFILE

    pushd $Rfolder
    for geocode in ${geocodes[@]}; do
        ## nowcasting
        # ATENÇÃO: se UPDATE_GIT_DATA_REPO for FALSE dados *não são* salvos,
        # permanecem como cópia local, suja. Se deseja limpar, pode rodar
        # depois:
        # cd $absoutfolder/outputs; git clean -f
        # que *apaga* todos arquivos untracked (DANGER)
        Rscript update_nowcasting.R --dir $absdatafolder/dados --escala $escala --sigla $estado --geocode $geocode --dataBase $today_ --outputDir $absoutfolder --trim $trim --updateGit $UPDATE_GIT_DATA_REPO

        ## mandando pro site
        path="${folder}/${estado}/${nomes[$geocode]}"
        
        # atualiza repo site
        # isto é feito a cada passo do loop - mais seguro?
        pushd $SCRIPTROOT/../site/_src
        git pull --ff-only

        # cria destino se não existe
        if [ ! -d ../dados/$path/tabelas_nowcasting_para_grafico/ ]; then
            mkdir --parents ../dados/$path/tabelas_nowcasting_para_grafico
        fi

        pushd $absoutfolder/$path/tabelas_nowcasting_para_grafico/
        cp $output_files $SCRIPTROOT/../site/dados/$path/tabelas_nowcasting_para_grafico/
        popd

        Rscript update_plots_nowcasting.R --escala $escala --sigla $estado --geocode $geocode --dataBase $today_
        pushd ../dados/$path/tabelas_nowcasting_para_grafico/
        git add $output_files
        popd
        pushd ../web/$path
        git add $web_output_files
        popd
        git commit -m ":robot: novas tabelas e plots ${escala} ${estado}-${nomes[$geocode]} ${today_}" &&
        git push
        popd
    done
    popd

#    ## Isto só é necessário se o outfolder for dentro de um submodulo
#    if [ $UPDATE_GIT_DATA_REPO == "TRUE" ]; then
#        # update meta-repo pro novo commit
#        git commit ../dados/estado_$estado -m ":robot: Atualizando commit estado ${estado}" &&
#        git push
#    fi

    rm $RUNFILE
fi
