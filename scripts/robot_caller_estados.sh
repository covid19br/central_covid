#!/bin/bash

RUNFILE="nowcasting_robot_caller_estados.run"
trim=5

if [ -f $RUNFILE ]; then
    exit
fi

touch $RUNFILE

source functions.sh

for geocode in ${!ESTADOS[@]}; do
    echo "Rodando ./auto_site_escala.sh estado ${ESTADOS[$geocode]} $trim $geocode"
    ./auto_site_escala.sh estado ${ESTADOS[$geocode]} $trim $geocode
done
    
rm $RUNFILE
