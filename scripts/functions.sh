#!/bin/bash

# - recebe padrão, com "{data}" no lugar do campo data em formato
#   YYYY_MM_DD e * para trechos livres
# - retorna maior data
#
# cuidado: argumento precisa ter aspas *simples* pra não ser expandido
# data deve vir no formato YYYY_MM_DD no nome do arquivo
get_latest(){
    local pattern=`basename "$1"`
    local lspat=`echo $1 | sed 's/{data}/*/'`
    local greppat=`echo $pattern | sed 's/*/.*/g; s/{data}/.*/'`
    local sedpat=`echo $pattern | sed 's/*/.*/g; s/{data}/\\\\([0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]\\\\)/'`
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
    for geocode in $3; do
        if [ $2 == "municipio" ]; then
            my_local_array[$geocode]=`awk -F, '/'"$geocode"'/ {gsub(/"/, "", $13); print $13}' ../nowcasting/dados/geocode_ibge.csv`
        elif [ $2 == "drs" ]; then
            # TODO: DRS de outros estados
            my_local_array[$geocode]=`awk -F, '{ if($2 == '"$geocode"') {gsub(/"/, "", $5); print $5}}' ../nowcasting/dados/DRS_SP.csv | head -n1`
        elif [ $2 == "meso" ]; then
            my_local_array[$geocode]=`awk -F, '{if($5 == '"$geocode"') {gsub(/ /, "_"); gsub(/"/, ""); print $6; exit}}' ../nowcasting/dados/geocode_ibge.csv | iconv -f utf8 -t ascii//TRANSLIT -`
        elif [ $2 == "micro" ]; then
            my_local_array[$geocode]=`awk -F, '{if($3 == '"$geocode"') {gsub(/ /, "_"); gsub(/"/, ""); print $4; exit}}' ../nowcasting/dados/geocode_ibge.csv | iconv -f utf8 -t ascii//TRANSLIT -`
        fi
    done
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
