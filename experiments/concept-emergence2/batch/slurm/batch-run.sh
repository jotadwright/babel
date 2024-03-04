#!/bin/bash

for i in $(seq $1);
do
    sbatch slurm/paper-hydra.sh
done