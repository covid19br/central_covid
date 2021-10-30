#!/bin/bash

source functions.sh

METAREPO=`get_abspath ..`
# script assume que o meta-repo tem a estrutura usual:
# nowcasting: repo nowcasting
# site: repo site

# update site repo and look for the trigger commit
cd $METAREPO/site
git pull --ff-only --commit
trigger=`git log --since=yesterday --pretty=oneline | grep "automatic nowcasting started"`

if [ -z "$trigger" ]; then
    echo "Sem commit de disparo ainda, saindo..."
    exit 0
fi

# get date from the commit message
date=`echo $trigger | sed 's/.*started \(20[0-9][0-9]_[0-9][0-9]_[0-9][0-9]\).*/\1/g'`
if [ ${#date} -ne 10 ]; then
    echo "Data não reconhecida, saindo..."
    exit 0
fi

# check for starter commit
git log --since=yesterday --pretty=oneline | grep ":robot: update plots Brasil $date"
if [ $? -eq 0 ]; then
    echo "Robô já rodou, saindo..."
    exit 0
fi

# run the stuff
cd $METAREPO/site/_src
Rscript update_plots_brasil.R
if [ $? -eq 0 ]; then
    # commit new plots
    cd ../web/brasil
    git pull --ff-only --commit && 
        git commit plot_nowcast_{ob_,}{srag,covid}.png -m ":robot: update plots Brasil $date" &&
        git push
fi

