#!/bin/bash

RUNFILE="nowcasting_robot_caller.run"
trim=5

if [ -f $RUNFILE ]; then
    exit
fi

touch $RUNFILE

source functions.sh
declare -A estados=()

while read -r estado geocode; do
    estado_geo=`get_estado municipio $geocode`
    if [ -z $estado_geo ]; then
        echo "Município $geocode não encontrado"
        rm $RUNFILE
        exit 1
    elif [ "$estado_geo" != "$estado" ]; then
        echo "Município $geocode pertence a ${estado_geo}, não a $estado"
        rm $RUNFILE
        exit 1
    fi
    estados[$estado]="${estados[$estado]} $geocode"
done <<<`cat municipios.txt`

IFS=$'\n' estados_ord=($(sort <<<"${!estados[*]}"))
unset IFS
for estado in "${estados_ord[@]}"; do
    echo "Rodando ./auto_site_escala.sh municipio $estado $trim ${estados[$estado]}"
    ./auto_site_escala.sh municipio $estado $trim ${estados[$estado]}
done

rm $RUNFILE
