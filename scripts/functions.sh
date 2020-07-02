#!/bin/bash

# - recebe padrão, com "{data}" no lugar do campo data em formato
#   YYYY_MM_DD e * para trechos livres
# - retorna maior data
#
# cuidado: argumento precisa ter aspas *simples* pra não ser expandido
# data deve vir no formato YYYY_MM_DD no nome do arquivo
get_latest(){
    local pattern=`basename "$1"`
    local lspat=`echo $1 | sed 's/{data\(dash\)*}/*/'`
    local greppat=`echo $pattern | sed 's/*/.*/g; s/{data\(dash\)*}/.*/'`
    local sedpat=`echo $pattern | sed 's/*/.*/g; s/{data}/\\\\([0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]\\\\)/; s/{datadash}/\\\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\\\)/'`
    ls `eval echo $lspat` | grep "$greppat" | sed  's/^.*'$sedpat'/\1/' | sort -gr | head -n 1
}

# recebe escala e retorna nome da pasta correspondente
get_folder(){
    if [ $1 == "municipio" ]; then
        echo "municipios"
    elif [ $1 == "drs" ]; then
        echo "DRS"
    elif [ $1 == "meso" ]; then
        echo "mesorregioes"
    elif [ $1 == "micro" ]; then
        echo "microrregioes"
    fi
}

# recebe:
#   - nome da variável (associative array declarado com declare -A)
#   - escala
#   - lista de geocodes
# preenche array com nomes associados a cada geocode
# 
# requer iconv para escalas micro e meso
get_names(){
    declare -n my_local_array="$1"
    local escala=$2
    shift 2
    for geocode in $@; do
        if [ $escala == "municipio" ]; then
            my_local_array[$geocode]=`awk -F, '/^'"$geocode"'/ {gsub(/"/, "", $13); print $13}' ../nowcasting/dados/geocode_ibge.csv`
        elif [ $escala == "drs" ]; then
            # TODO: DRS de outros estados
            my_local_array[$geocode]=`awk -F, '{ if($2 == '"$geocode"') {gsub(/"/, "", $5); print $5}}' ../nowcasting/dados/DRS_SP.csv | head -n1`
        elif [ $escala == "meso" ]; then
            my_local_array[$geocode]=`awk -F, '{if($5 == '"$geocode"') {gsub(/ /, "_"); gsub(/"/, ""); print $6; exit}}' ../nowcasting/dados/geocode_ibge.csv | iconv -f utf8 -t ascii//TRANSLIT -`
        elif [ $escala == "micro" ]; then
            my_local_array[$geocode]=`awk -F, '{if($3 == '"$geocode"') {gsub(/ /, "_"); gsub(/"/, ""); print $4; exit}}' ../nowcasting/dados/geocode_ibge.csv | iconv -f utf8 -t ascii//TRANSLIT -`
        fi
    done
}

# recebe
#   - escala (municipio|micro|meso)
#   - geocode
# retorna sigla do estado
#
# não funciona pra DRS, só municipio, micro e meso
get_estado(){
    if [ $1 == "municipio" ]; then
        awk -F, '/'"$geocode"'/ {gsub(/"/, "", $8); print $8}' ../nowcasting/dados/geocode_ibge.csv | head -n1
    elif [ $1 == "meso" ]; then
        awk -F, '{ if($5 == '"$2"') {gsub(/"/, "", $8); print $8}}' ../nowcasting/dados/geocode_ibge.csv | head -n1
    elif [ $1 == "micro" ]; then
        awk -F, '{ if($3 == '"$2"') {gsub(/"/, "", $8); print $8}}' ../nowcasting/dados/geocode_ibge.csv | head -n1
    fi
}

# recebe lista de patterns (ver doc get_latest)
# retorna maior data e *pasta* (não pattern) correspondente
compare_get_latest(){
    local latest=""
    for i in "$@"; do
        local last_input=`get_latest ${i}`
        if [[ $last_input > $latest ]]; then
            local latest=$last_input
            local datafolder=`dirname "$i"`
        fi
    done
    echo "$latest $datafolder"
}

# recebe endereço (absoluto ou relativo)
# retorna endereço absoluto
get_abspath(){
    if [ ${1:0:1} = '/' ]; then
        echo $1
    else
        echo "$PWD/$1"
    fi
}
