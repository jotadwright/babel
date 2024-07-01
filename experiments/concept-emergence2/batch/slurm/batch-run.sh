#!/bin/bash

for i in $(seq $1);
do
    sbatch slurm/$2.sh
    sleep 60
done