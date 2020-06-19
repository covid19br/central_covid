#!/bin/bash

# script assume que é rodado dentro do meta-repo, numa pasta abaixo da raiz
# ../nowcasting: repo nowcasting
# ../site: repo site

usage(){
    echo -e "
USO: $0 [escala] {geocodes}

  escala: municipio | micro | meso | drs 
  geocodes: lista de geocodes, ou ids de DRSs
"
}

if [ ${#@} -lt 2 ]; then
    usage
    exit 1
fi

SCRIPTROOT=$PWD
source functions.sh

escala=$1
shift
geocodes=( "$@" )

Rfolder="../nowcasting"
SITEfolder="../site"
abssitefolder=`get_abspath $SITEfolder`
outfolder="$SITEfolder/dados"
absoutfolder=`get_abspath $outfolder`

folder=`get_folder $escala`
if [ -z $folder ]; then
    usage
    exit 1
fi

# escala = drs assume estado = SP
declare -A nomes=()
get_names nomes $escala "${geocodes[@]}"

# pega o estado do 1o geocode
# ATENÇÃO: não misturar municípios de estados diferentes!
estado=`get_estado $escala ${geocodes[1]}`

## pastas de dados
# cada estado pode ter uma base diferente
dados_estado="../dados/estado_${estado}/SRAG_hospitalizados/dados"

# pull do meta-repo: *DANGER HERE*
# este pull é pra você poder atualizar o meta-repo depois - se tiver base nova
# o commit do submodulo dele estará *desatualizado*
git pull --recurse-submodules --ff-only
if [ -d $dados_estado ]; then
    pushd $dados_estado
    # AQUI pegamos alterações novas, sem detached HEAD no submodule
    git checkout master && git pull --ff-only
    popd
fi

read last_input datafolder < <(
    compare_get_latest "$dados_estado/SRAGH*_{data}.csv" \
                       "$dados_estado/SRAGH*_{data}.zip" \
                       "../dados/SIVEP-Gripe/SRAGH*_{data}.zip")
absdatafolder=`get_abspath $datafolder`
today_=$last_input
todaydash=`echo $today_ | sed 's/_/-/g'`

RUNFILE="modelogro_site_${escala}_${estado}.run"
if [[ -f $RUNFILE  ]]; then
    # rodando
    exit
fi
touch $RUNFILE

# esta é arcana...
fit_output_files="curve_fits_${todaydash}.Rds"
reports_files="${todaydash}_relatorio_projecoes_demanda_hospitalar_{srag,covid}.pdf"
hosp_output_files="hopitalized_UTI_${todaydash}.csv 
hopitalized_${todaydash}.csv 
${todaydash}_internacoes_por_dia_{covid,srag}.csv"
web_output_files="last.update.modelogro.txt
plot.{covid,srag}.{leitos,uti}.forecast.{exp,logistic}.{,lg.,md.,sm.,ex.}svg
plot.{covid,srag}.{leitos,uti}.forecast.{exp,logistic}.html"
# eval expande todos wildcards nas variáveis
hosp_output_files=`eval echo $hosp_output_files`
reports_files=`eval echo $reports_files`
web_output_files=`eval echo $web_output_files`

pushd $Rfolder
for geocode in ${geocodes[@]}; do
    if [ -z ${nomes[$geocode]} ]; then
        echo $escala $geocode não encontrado
        rm $RUNFILE
        exit 1
    fi

    munpath="projecao_leitos/${folder}/${estado}/${nomes[$geocode]}"

    last_output=`get_latest ${SITEfolder}'/dados/'${munpath}'/relatorios/{datadash}_relatorio_projecoes_demanda_hospitalar_srag.pdf'`
    last_output=`echo $last_output | sed 's/-/_/g'`
    if [[ ! $last_output < $last_input ]]; then
        continue
    fi

    echo "Nova atualização modelogro ${today_} ${escala} ${estado} ${nomes[$geocode]}"

    ## nowcasting
    Rscript update_projecao_leitos.R --dir $absdatafolder --escala $escala --sigla SP --geocode $geocode --dataInicial "2020-03-08" --out_dir $absoutfolder --check_report TRUE

    ## mandando pro site
    # atualiza repo site
    # isto é feito a cada passo do loop - mais seguro?
    pushd $SITEfolder/_src
    Rscript update_modelogro.R --escala $escala --sigla $estado --geocode $geocode

    git pull --ff-only

    pushd ../dados/$munpath/hospitalizados
    git add $hosp_output_files
    popd
    
    pushd ../dados/$munpath/curve_fits
    git add $fit_output_files
    popd
    
    pushd ../dados/$munpath/relatorios
    cp ${todaydash}_relatorio_projecoes_demanda_hospitalar_srag.pdf relatorio_demanda_hospitalar_srag.pdf
    git add $reports_files relatorio_demanda_hospitalar_srag.pdf
    popd
    
    # pushd ../dados/$munpath/figures
    # git add $reports_files
    # popd

    pushd ../web/$munpath
    git add $web_output_files
    popd
    
    git commit -m ":robot: novas projeções de leitos e plots $escala ${estado}-${nomes[$geocode]} ${today_}" &&
    git push
    popd
done
popd

rm $RUNFILE
