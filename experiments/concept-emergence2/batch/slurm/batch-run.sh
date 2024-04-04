#!/bin/bash

for i in $(seq $1);
do
    sleep 2
    sbatch slurm/$2.sh
done