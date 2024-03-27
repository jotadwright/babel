#!/bin/bash

for i in $(seq $1);
do
    sbatch slurm/ce3-runtime.sh
done