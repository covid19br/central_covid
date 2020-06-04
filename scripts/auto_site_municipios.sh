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


# TODO: discutir tamanho do trim
trim=2
# atualiza repo onde dados estão?
UPDATE_GIT_DATA_REPO=TRUE

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
out="../site/dados/municipios/${estado}/${nomes_municipios[${municipios[0]}]}/tabelas_nowcasting_para_grafico/nowcasting_acumulado_covid_${today_}.csv"
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
    for geocode in ${municipios[@]}; do
        ## nowcasting
        # ATENÇÃO: se UPDATE_GIT_DATA_REPO for FALSE dados *não são* salvos,
        # permanecem como cópia local, suja. Se deseja limpar, pode rodar
        # depois:
        # cd $absoutfolder/outputs; git clean -f
        # que *apaga* todos arquivos untracked (DANGER)
        Rscript update_nowcasting.R --dir $absdatafolder/dados --escala municipio --geocode $geocode --dataBase $today_ --outputDir $absoutfolder --trim $trim --updateGit $UPDATE_GIT_DATA_REPO

        ## mandando pro site
        munpath="municipios/${estado}/${nomes_municipios[$geocode]}"
        
        # atualiza repo site
        # isto é feito a cada passo do loop - mais seguro?
        pushd $SCRIPTROOT/../site/_src
        git pull --ff-only

        # cria destino se não existe
        if [ ! -d ../dados/$munpath/tabelas_nowcasting_para_grafico/ ]; then
            mkdir --parents ../dados/$munpath/tabelas_nowcasting_para_grafico
        fi

        pushd $absoutfolder/$munpath/tabelas_nowcasting_para_grafico/
        cp $output_files $SCRIPTROOT/../site/dados/$munpath/tabelas_nowcasting_para_grafico/
        popd

        Rscript update_plots_nowcasting.R --escala municipio --geocode $geocode --dataBase $today_
        pushd ../dados/$munpath/tabelas_nowcasting_para_grafico/
        git add $output_files
        popd
        pushd ../web/$munpath
        git add $web_output_files
        popd
        git commit -m ":robot: novas tabelas e plots ${estado}-${nomes_municipios[$geocode]} ${today_}" &&
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
