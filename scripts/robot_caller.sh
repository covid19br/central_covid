#!/bin/bash

RUNFILE="nowcasting_robot_caller.run"

if [ -f $RUNFILE ]; then
    exit
fi

touch $RUNFILE

cat municipios.txt | while read line; do
    estado=${line%% *}
    geocode=${line#* }
    echo "Rodando ./auto_site_escala.sh municipio $estado 2 $geocode"
    ./auto_site_escala.sh municipio $estado 2 $geocode
done

rm $RUNFILE
