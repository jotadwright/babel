#!/bin/bash

for i in $(seq $2 $3);
do
    screen -S $1_$i -X quit
done