#!/bin/bash

# script assume que é rodado dentro do meta-repo, numa pasta abaixo da raiz
# ../nowcasting: repo nowcasting

estado="SP"
nDRS=`seq 1 17`

# associative array!
declare -A nomes_DRS
for drs in ${nDRS}; do
    nomes_DRS[$drs]=`awk -F, '{ if($2 == '"$drs"') {gsub(/"/, "", $5); print $5}}' ../nowcasting/dados/DRS_SP.csv | head -n1`
done

datafolder="../dados/estado_${estado}/SRAG_hospitalizados"
Rfolder="../nowcasting"
if [ ${datafolder:0:1} = '/' ]; then
    absdatafolder=$datafolder
else
    absdatafolder="$PWD/$datafolder"
fi

today=`LANG=en date +'%b %-d'`
today_=`date +'%Y_%m_%d'`
todaydash=`date +'%Y-%m-%d'`

# csv pode já ter sido processado
#csv="$absdatafolder/dados/Pacientes_internados_com_SRAG_data${todaydash}.csv"
csv2="$absdatafolder/dados/SRAGH_${today_}.csv"
out="$absdatafolder/reports/projecao_leitos_srag_${todaydash}.pdf"
RUNFILE="projecao_leitos_DRS_${estado}.run"

# pull do meta-repo: *DANGER HERE*
# este pull é pra você poder atualizar o meta-repo depois - se tiver base nova
# o commit do submodulo dele estará *desatualizado*
git pull --recurse-submodules --ff-only
pushd $absdatafolder
# AQUI pegamos alterações novas, sem detached HEAD no submodule
git checkout master && git pull --ff-only &&
git log -- dados/ | grep  "$today"
newcommit=$?
popd

if [[ $newcommit && -f $csv2 && ! -f $out && ! -f $RUNFILE ]]; then
    touch $RUNFILE

    ## nowcasting
    pushd $Rfolder
    for DRS in $nDRS; do
        Rscript update_projecao_leitos.R --dir $absdatafolder/dados --escala drs --sigla $estado --geocode $DRS --dataBase $today_ --dataInicial 2020-03-16 --out_dir $absdatafolder/outputs/ --check_report TRUE &&
        cd $absdatafolder
        git pull &&
        git add outputs/projecao_leitos/DRS/$estado/${nomes_DRS[$DRS]}/curve_fits/curve_fits_${todaydash}.Rds &&
        git add outputs/projecao_leitos/DRS/$estado/${nomes_DRS[$DRS]}/hospitalizados/hopitalized_${todaydash}.csv &&
        git add outputs/projecao_leitos/DRS/$estado/${nomes_DRS[$DRS]}/hospitalizados/hopitalized_UTI_${todaydash}.csv &&
        git add relatorios/${todaydash}_relatorio_projecoes_demanda_hospitalar_{srag,covid}.pdf &&
        git commit -m ":robot: projecao leitos DRS ${estado}-${nomes_DRS[$DRS]}" &&
        # DANGER: rebase é perigo: mantenha sua cópia local em ordem!
        # por outro lado, é a única solução com robôs concorrentes em outra máquina.
        git pull --rebase &&
        git push
        cd -
    done
    popd

    pushd $absdatafolder/outputs/
    # gera relatório unificado
    pdfunite projecao_leitos/DRS/$estado/*/relatorios/${todaydash}_relatorio_projecoes_demanda_hospitalar_srag.pdf ../reports/projecao_leitos_srag_${todaydash}.pdf &&
    git pull &&
    git add ../reports/projecao_leitos_srag_${todaydash}.pdf &&
    git commit -m ":robot: relatório unificado projecao leitos ${todaydash}" &&
    git push
    popd

    # atualizando meta-repo (de novo)
    # DANGER: altíssimo risco de conflito?
    # tentando escapar ao máximo...
    git pull --recurse-submodules --ff-only
    pushd $absdatafolder
    # pegamos alterações novas, sem detached HEAD no submodule
    git checkout master && git pull --ff-only
    popd
    git commit ../dados/estado_$estado -m ":robot: Atualizando commit estado ${estado}" &&
    git push

    rm $RUNFILE
fi
