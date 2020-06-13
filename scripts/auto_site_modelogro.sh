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

# careful: argument must be inside *single* quotes to avoid being expanded
get_latest(){
    pattern=`basename "$1"`
    greppat=`echo $pattern | sed 's/*/.*/'`
    sedpat=`echo $pattern | sed 's/*/\\\\(.*\\\\)/'`
    ls `eval echo $1` | grep "$greppat" | xargs -I{} basename {} | sed  's/'$sedpat'/\1/' | sort -gr | head -n 1
}

if [ ${#@} -lt 2 ]; then
    usage
    exit 1
fi

SCRIPTROOT=$PWD

estado="SP"

escala=$1
shift
geocodes=( "$@" )

datafolder="../dados/estado_${estado}/SRAG_hospitalizados/dados"
#datafolder="../dados/SIVEP-Gripe"
Rfolder="../nowcasting"
# convertendo caminhos relativos em absolutos
# realpath é mais profissa, mas não é garantido ter em todo lugar
if [ ${datafolder:0:1} = '/' ]; then
    absdatafolder=$datafolder
else
    absdatafolder="$PWD/$datafolder"
fi

if [ $escala == "municipio" ]; then
    folder="municipios"
elif [ $escala == "drs" ]; then
    folder="DRS"
elif [ $escala == "meso" ]; then
    folder="mesorregioes"
elif [ $escala == "micro" ]; then
    folder="microrregioes"
else
    usage
    exit 1
fi

# associative array of names
# TODO: testar micro- e mesorregiões
# requires iconv
declare -A nomes
for geocode in ${geocodes[@]}; do
    if [ $escala == "municipio" ]; then
        nomes[$geocode]=`awk -F, '/'"$geocode"'/ {gsub(/"/, "", $13); print $13}' ../nowcasting/dados/geocode_ibge.csv`
    elif [ $escala == "drs" ]; then
        nomes[$geocode]=`awk -F, '{ if($2 == '"$geocode"') {gsub(/"/, "", $5); print $5}}' ../nowcasting/dados/DRS_SP.csv | head -n1`
    elif [ $escala == "meso" ]; then
        nomes[$geocode]=`awk -F, '{if($5 == '"$geocode"') {gsub(/ /, "_"); gsub(/"/, ""); print $6; exit}}' ../nowcasting/dados/geocode_ibge.csv | iconv -f utf8 -t ascii//TRANSLIT -`
    elif [ $escala == "micro" ]; then
        nomes[$geocode]=`awk -F, '{if($3 == '"$geocode"') {gsub(/ /, "_"); gsub(/"/, ""); print $4; exit}}' ../nowcasting/dados/geocode_ibge.csv | iconv -f utf8 -t ascii//TRANSLIT -`
    fi
done

# pull do meta-repo: *DANGER HERE*
# este pull é pra você poder atualizar o meta-repo depois - se tiver base nova
# o commit do submodulo dele estará *desatualizado*
git pull --recurse-submodules --ff-only
pushd $absdatafolder
# AQUI pegamos alterações novas, sem detached HEAD no submodule
git checkout master && git pull --ff-only
popd

RUNFILE="modelogro_site_${escala}_${estado}.run"
last_input=`get_latest '$absdatafolder/SRAGH_*.zip'`
#last_input=`get_latest '$absdatafolder/SRAGHospitalizado_*.zip'`
last_output=`get_latest '../site/dados/projecao_leitos/'${folder}'/'${estado}'/'${nomes[${geocodes[0]}]}'/relatorios/*_relatorio_projecoes_demanda_hospitalar_srag.pdf'`

last_output=`echo $last_output | sed 's/-/_/g'`

if [[ -f $RUNFILE || ! $last_output < $last_input ]]; then
    # rodando ou atualizado
    exit
fi

today_=$last_input
todaydash=`echo $today_ | sed 's/_/-/g'`

echo "Nova atualização modelogro ${today_} ${escala} ${estado} ${geocodes[@]}"

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

touch $RUNFILE

pushd $Rfolder
for geocode in ${geocodes[@]}; do
    ## nowcasting
    Rscript update_projecao_leitos.R --dir $absdatafolder --escala $escala --sigla $estado --geocode $geocode --dataInicial "2020-03-08" --out_dir ../site/dados/ --check_report TRUE

    ## mandando pro site
    munpath="projecao_leitos/${folder}/${estado}/${nomes[$geocode]}"
    
    # atualiza repo site
    # isto é feito a cada passo do loop - mais seguro?
    pushd $SCRIPTROOT/../site/_src
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
