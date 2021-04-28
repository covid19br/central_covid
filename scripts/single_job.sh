#!/bin/bash

if [ ${#@} -lt 6 ]; then
    echo "Faltando parametros. Esperava:"
    echo "$0 pasta_de_dados data escala sigla_estado geocode nome"
    exit 1
fi

source functions.sh

METAREPO=`get_abspath ..`
# script assume que o meta-repo tem a estrutura usual:
# nowcasting: repo nowcasting
# site: repo site

trim=5
# método de cálculo de R efetivo = Cori | old_Cori
RMETHOD="Cori"
# n cores
NCORES=2
# atualiza repo onde dados estão?
UPDATE_GIT_DATA_REPO=FALSE

absdatafolder=$1
data=$2
escala=$3
estado=$4
geocode=$5
nome=$6

abssitefolder=$METAREPO/site
absoutfolder=$METAREPO/dados_processados/nowcasting
LOGFILE=$METAREPO/../logs/"${escala}_${estado}_${nome}.log"

cd $METAREPO/scripts
source functions.sh

# esta é arcana...
output_files="nowcasting_{acumulado,diario}_{,obitos_}{covid,srag}_${data}.csv
r_efetivo_{covid,srag}_${data}.csv"
web_output_files="last.update.txt
data_forecast_exp_{,obitos_}{covid,srag}.csv
data_Re_{covid,srag}.csv
plot_nowcast_{,cum_}{,ob_}{covid,srag}.html
plot_nowcast_{,cum_}{,ob_}{covid,srag}{,.ex,.lg,.md,.sm}.svg
plot_estimate_R0_{covid,srag}.html
plot_estimate_R0_{covid,srag}{,.ex,.lg,.md,.sm}.svg
data_atualizacao_{,obitos_}{covid,srag}.csv"
## arquivos relacionados a TD descontinuados
# tempo_duplicacao_{,obitos_}{covid,srag}_${data}.csv
# data_tempo_dupli_{,obitos_}{covid,srag}.csv
# plot_tempo_dupl_{,ob_}{covid,srag}.html
# plot_tempo_dupl_{,ob_}{covid,srag}{,.ex,.lg,.md,.sm}.svg

# eval expande todos wildcards nas variáveis
output_files=`eval echo $output_files`
web_output_files=`eval echo $web_output_files`

folder=`get_folder $escala`
if [ $escala == "estado" ]; then
    path="${folder}/${estado}"
else
    path="${folder}/${estado}/${nome}"
fi

cd $METAREPO/nowcasting
#Rscript update_nowcasting.R --dir $absdatafolder --escala $escala --sigla $estado --geocode $geocode --dataBase $data --outputDir $absoutfolder --trim $trim --updateGit $UPDATE_GIT_DATA_REPO --Rmethod $RMETHOD --ncores $NCORES &> $LOGFILE
Rscript update_nowcasting.R --dir $absdatafolder --escala $escala --sigla $estado --geocode $geocode --dataBase $data --outputDir $absoutfolder --trim 0 --window 10 --semanal TRUE --updateGit $UPDATE_GIT_DATA_REPO --Rmethod $RMETHOD --ncores $NCORES &> $LOGFILE
if [ $? != 0 ]; then
    echo "update_nowcasting falhou para $estado $nome $data. Saindo..." >> $LOGFILE 2>&1
    exit 1
fi

## mandando pro site

# atualiza repo site
cd $METAREPO/site/_src

# cria destino se não existe
if [ ! -d $abssitefolder/dados/$path/tabelas_nowcasting_para_grafico/ ]; then
    mkdir --parents $abssitefolder/dados/$path/tabelas_nowcasting_para_grafico
fi

pushd $absoutfolder/$path/tabelas_nowcasting_para_grafico/
cp $output_files $abssitefolder/dados/$path/tabelas_nowcasting_para_grafico/
popd

echo "===== UPDATE PLOTS SITE =====" >> $LOGFILE
Rscript update_plots_nowcasting.R --escala $escala --sigla $estado --geocode $geocode --dataBase $data >> $LOGFILE 2>&1
if [ $? != 0 ]; then
    echo "update_plots falhou para $estado $nome. Saindo..." >> $LOGFILE 2>&1
    exit 1
fi

## DANGER
# isto é feito a cada passo do loop - mais seguro?
git pull --ff-only

BUSYGIT=$METAREPO/scripts/busygit.tmp
# poor man's semaphore
while [ -f $BUSYGIT ]; do sleep 1; done
touch $BUSYGIT
pushd $abssitefolder/dados/$path/tabelas_nowcasting_para_grafico/
git add $output_files >> $LOGFILE 2>&1
popd
pushd $abssitefolder/web/$path
git add $web_output_files >> $LOGFILE 2>&1
popd
git commit -m ":robot: novas tabelas e plots ${escala} ${estado}-${nome} ${data}" &&
git push >> $LOGFILE 2>&1
rm $BUSYGIT

