#!/bin/bash

for i in $(seq $2 $3);
do
    echo "run.SH: $(pwd)"
    screen -S $1_$i -dm bash bash/scripts/$1_$i.sh 
done