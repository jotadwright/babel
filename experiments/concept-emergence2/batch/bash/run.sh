#!/bin/bash

for i in $(seq $2 $3);
do
    screen -S $1_$i -dm bash bash/scripts/$1_$i.sh 
done