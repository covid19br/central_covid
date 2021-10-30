#!/bin/bash

## CONFIGURAÇÂO E PASTAS

# script assume que o meta-repo tem a estrutura usual
SCRIPT_FOLDER=`dirname $0`
cd $SCRIPT_FOLDER

METAREPO=`readlink -f ..`

DADOS=$METAREPO/dados/SIVEP-Gripe
SITE=$METAREPO/site
NOWCAST=$METAREPO/nowcasting
NOWCAST2=$METAREPO/nowcasting2


## FUNÇÔES

integridade(){
    # integridade sem estrutura etária
    pushd $NOWCAST
    Rscript checa_base.R --updateGit TRUE

    # integridade com estrutura etária
    pushd $METAREPO/dados_processados/integridade_SIVEP
    for i in age_dados_{,ob}{covid,srag}_{br,est}.csv.xz; do
        unxz $i
    done
    popd

    Rscript diff_bases_idade.R

    pushd $METAREPO/dados_processados/integridade_SIVEP
    for i in age_dados_{,ob}{covid,srag}_{br,est}.csv; do
        xz $i
    done

    git commit age_db.info.csv age_dados_{,ob}{covid,srag}_{br,est}.csv.xz -m ":robot: atualizando diff de bases por idade $1" &&
    git push

    popd
    popd
}

sumario_SIVEP(){
    pushd $NOWCAST2
    Rscript summary_database.R  --dataBase $1 --updateGit TRUE
    popd
}

nowcast(){
    pushd $SCRIPT_FOLDER
    date=$1

    if [ ${#@} -gt 1 ]; then
        NPJOBS=$2
    else
        NPJOBS=10
    fi

    LISTA_JOBS=lista_jobs.txt

    # make starter commit
    git commit --allow-empty -m ":robot: automatic nowcasting started $date" && git push

    echo "Rodando: parallel -a $LISTA_JOBS --colsep ' ' -j $NPJOBS ./single_job.sh $DADOS $date"
    parallel -a $LISTA_JOBS --colsep ' ' -j $NPJOBS ./single_job.sh $DATAFOLDER $data

    popd
}


## MAIN

# DOWNLOAD
/usr/bin/python3 sivep_downloader.py >> ~/.cache/covid-sivep-down.log 2>&1

# se tem base SIVEP nova
if [ $? -eq 0 ]; then
    echo "Nova base SIVEP"
    commit=`git log --since=1am --pretty=oneline | grep "base SIVEP-Gripe de"`
    date=`echo $commit | sed 's/.*Gripe de \(20[0-9][0-9]_[0-9][0-9]_[0-9][0-9]\).*/\1/g'`
    echo "data da base: $date"

    # deixando apenas base descompactada
    FNAME="SRAGHospitalizado_${date}.csv"
    cd $DADOS
    mv ${FNAME}.xz "${FNAME}.21.xz" tmp/

    integridade $date & sumario_SIVEP $date

    nowcast $date

    # limpando
    cd $DADOS/tmp
    mv ${FNAME}.xz "${FNAME}.21.xz" ../
    rm ${FNAME}
fi
