#!/bin/bash


git pull --ff-only --commit || (echo "Erro no pull, saindo..."; exit 1)

last_update=`git log --pretty=oneline output/relatorio_InfogripeXObservatorio_nowcastings_semanais.html | grep ":robot: atualizando relatório comparação de nowcastings" | head -n 1`

if [ -z "$last_update" ]; then
    # no robots before:
    date="2021_05_22"
else
    # get date from the commit message
    date=`echo $last_update | sed 's/.*nowcastings \(20[0-9][0-9]_[0-9][0-9]_[0-9][0-9]\).*/\1/g'`
    if [ ${#date} -ne 10 ]; then
        echo "Data não reconhecida, saindo..."
        exit 0
    fi
fi

Rscript compara_obs_semanal_com_infogripe.R --auto ${date}

if [ $? -eq 0 ]; then
    git pull --ff-only --commit || (echo "Erro no pull, saindo..."; exit 1)
    git commit output/relatorio_InfogripeXObservatorio_nowcastings_semanais.html -m ":robot: atualizando relatório comparação de nowcastings `date +%Y_%m_%d`" &&
    git push &&
    echo -e "O relatório de comparação de nowcastings foi atualizado.\n
Ele já está disponível para download no link https://github.com/covid19br/central_covid/raw/master/scripts_R_genericos/InfoGripe/output/relatorio_InfogripeXObservatorio_nowcastings_semanais.html.\n
Atenciosamente,\nRobot mailer" |
    mail -s "relatório de comparação de nowcastings atualizado" $(<emails.txt)
fi
