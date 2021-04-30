#!/bin/bash

source functions.sh

METAREPO=`get_abspath ..`
# script assume que o meta-repo tem a estrutura usual:
# nowcasting: repo nowcasting
# site: repo site

# update site repo and look for the trigger commit
cd $METAREPO/site
git pull --rebase || (git rebase --abort ; echo "Erro no pull, saindo..."; exit 1)
trigger=`git log --since=yesterday --pretty=oneline | grep "trigger nowcasting update"`

if [ -z $trigger ]; then
    echo "Sem commit de disparo ainda, saindo..."
    exit 0
fi

# get date from the commit message
date=`echo $trigger | sed 's/.*update \(20[0-9][0-9]_[0-9][0-9]_[0-9][0-9]\).*/\1/g'`
if [ ${#date} -ne 10 ]; then
    echo "Data não reconhecida, saindo..."
    exit 0
fi

# check for starter commit
git log --since=yesterday --pretty=oneline | grep ":robot: automatic nowcasting started $date"
if [ $? -eq 0 ]; then
    echo "Robô já rodou, saindo..."
    exit 0
fi

# make starter commit
git commit --allow-empty ":robot: automatic nowcasting started $date" && git push

# run the stuff
cd $METAREPO/scripts
./run_parallel.sh $date "todos" 11

