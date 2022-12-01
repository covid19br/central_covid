#!/bin/bash

if [ "$#" -eq 0 ]; then
    echo "erro: falta data"
    exit 1
else
    data=$1
fi

SITE="../site"
JOBS="lista_jobs.txt"
OUT="todo_${data}.txt"

pushd $SITE
TMP=`mktemp -p .`
git log --oneline | grep $data | awk '{print $5" "$6}' | sort > $TMP
popd

mv $SITE/$TMP .

if [ -f $OUT ]; then
    rm $OUT
fi

cat $JOBS | while read line; do
    PATTERN=`echo $line | awk '{print $1" "$2"-"$4}'`
    grep -q "$PATTERN" $TMP
    if [ $? -ne 0 ]; then
        echo $line >> $OUT
    fi
done

rm $TMP
