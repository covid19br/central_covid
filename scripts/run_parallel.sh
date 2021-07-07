#!/bin/bash

if [ ${#@} -lt 2 ]; then
    echo "Faltando parametros. Esperava:"
    echo "$0 data [SP|outros|todos]"
    exit 1
fi

RUNFILE="run_parallel.run"
if [ -f $RUNFILE ]; then
    echo "Script em execucao"
    exit 0
fi
touch $RUNFILE

source functions.sh

METAREPO=`get_abspath ..`
# script assume que o meta-repo tem a estrutura usual:
# nowcasting: repo nowcasting
# site: repo site

data=$1
task=$2

if [ ${#@} -gt 2 ]; then
    NPJOBS=$3
else
    NPJOBS=9
fi

if [ $task == "todos" ]; then
    LISTA_JOBS=lista_jobs.txt
    DATAFOLDER=$METAREPO/dados/SIVEP-Gripe
    pushd $DATAFOLDER
    wget "https://github.com/covid19br/central_covid/raw/master/dados/SIVEP-Gripe/SRAGHospitalizado_${data}.csv.xz"
    if [ $? != 0 ]; then
        echo "Erro baixando base SIVEP"
        rm $RUNFILE
        exit 1
    fi
    unxz -T 8 SRAGHospitalizado_${data}.csv.xz
    # a partir de 07/07/2021 a base vem quebrada
    wget "https://github.com/covid19br/central_covid/raw/master/dados/SIVEP-Gripe/SRAGHospitalizado_${data}.csv.21.xz"
    if [ $? == 0 ]; then
        unxz -T 8 SRAGHospitalizado_${data}.csv.21.xz
        tail -n +2 SRAGHospitalizado_${data}.csv.21 >> SRAGHospitalizado_${data}.csv
        rm SRAGHospitalizado_${data}.csv.21
    fi
    #rm SRAGHospitalizado_${data}.zip
elif [ $task == "outros" ]; then
    LISTA_JOBS=lista_jobs_outros.txt
    DATAFOLDER=$METAREPO/dados/SIVEP-Gripe
    pushd $DATAFOLDER
    wget "https://github.com/covid19br/central_covid/raw/master/dados/SIVEP-Gripe/SRAGHospitalizado_${data}.csv.xz"
    if [ $? != 0 ]; then
        echo "Erro baixando base SIVEP"
        rm $RUNFILE
        exit 1
    fi
    unxz -T 8 SRAGHospitalizado_${data}.csv.xz
    # a partir de 07/07/2021 a base vem quebrada
    wget "https://github.com/covid19br/central_covid/raw/master/dados/SIVEP-Gripe/SRAGHospitalizado_${data}.csv.21.xz"
    if [ $? == 0 ]; then
        unxz -T 8 SRAGHospitalizado_${data}.csv.21.xz
        tail -n +2 SRAGHospitalizado_${data}.csv.21 >> SRAGHospitalizado_${data}.csv
    fi

    #rm SRAGHospitalizado_${data}.zip
elif [ $task == "SP" ]; then
    LISTA_JOBS=lista_jobs_SP.txt
    DATAFOLDER=$METAREPO/dados/estado_SP/SRAG_hospitalizados/dados
    pushd $DATAFOLDER
    git pull --ff-only
    if [[ ! -f "SRAGH_${data}.csv" && ! -f "SRAGH_${data}.zip" ]]; then
        echo "Erro baixando base SIVEP"
        rm $RUNFILE
        exit 1
    fi
    if [ -f "SRAGH_${data}.zip" ]; then
        unzip "SRAGH_${data}.zip"
    fi
#    DATAFOLDER=$METAREPO/dados/SIVEP-Gripe
#    pushd $DATAFOLDER
#    wget "https://github.com/covid19br/central_covid/raw/master/dados/SIVEP-Gripe/SRAGHospitalizado_${data}.zip"
#    if [ $? != 0 ]; then
#        echo "Erro baixando base SIVEP"
#        rm $RUNFILE
#        exit 1
#    fi
#    unzip -o SRAGHospitalizado_${data}.zip
#    rm SRAGHospitalizado_${data}.zip
else
    echo "Parametro inesperado: $task (deve ser SP|outros|todos)"
    rm $RUNFILE
    exit 1
fi

popd

cd $METAREPO/scripts
date
echo "Rodando: parallel -a $LISTA_JOBS --colsep ' ' -j $NPJOBS ./single_job.sh $DATAFOLDER $data"
parallel -a $LISTA_JOBS --colsep ' ' -j $NPJOBS ./single_job.sh $DATAFOLDER $data

if [[ $task == "outros" || $task == "todos" ]]; then
    rm ${DATAFOLDER}/SRAGHospitalizado_${data}.csv
fi
rm $RUNFILE

