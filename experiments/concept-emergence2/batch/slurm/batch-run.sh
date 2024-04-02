#!/bin/bash

for i in $(seq $1);
do
    sleep 2
    sbatch slurm/ce3-runtime.sh
done