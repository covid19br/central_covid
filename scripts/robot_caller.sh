#!/bin/bash

cat municipios.txt | while read line; do
    estado=${line%% *}
    geocode=${line#* }
    echo "Rodando ./auto_site_escala.sh municipio $estado 2 $geocode"
    ./auto_site_escala.sh municipio $estado 2 $geocode
done
