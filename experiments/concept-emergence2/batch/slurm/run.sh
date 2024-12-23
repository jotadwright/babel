#!/bin/bash

mkdir -p "/user/brussel/101/vsc10156/concept-emergence2/batch/slurm/logs/${9}"

for i in $(seq $4 $5);
do
    epath="/user/brussel/101/vsc10156/concept-emergence2/batch/slurm/logs/${9}/${2}_%a_seed${i}_%A_e.txt"
    opath="/user/brussel/101/vsc10156/concept-emergence2/batch/slurm/logs/${9}/${2}_%a_seed${i}_%A_o.txt"
    sbatch --error $epath --output $opath --job-name "${9}-seed${i}-${1}" --array $3 --time $6 --mem $7  --ntasks 1 --cpus-per-task 1 --export=seed=$i,name=$2,space=$8,exp_top_dir=$9 slurm/$1.sh
done

# example usage: 
# arguments:
#    - $1 = name of the experiment (corresponds to the csv)
#    - $2 = in the csv; which sub-experiments to run
#    - $3 = starting seed
#    - $4 = last seed
#    - $5 = wall-time
#    - $6 = memory requirement
#    
# $ bash slurm/run.sh train paper-hydra 1,3,4 1 10 08:00:00 8G 10000 exp-name
#  -> this script runs sub-experiment 1,3,4 in paper-hydra.csv
#  -> it runs all those experiments with seeds 1 through 10 (inclusive)
#  -> it allocates 16GB and a walltime of 10 hours for each individual sub-experiment
#  -> it reserves a dynamic-space-size of 10000 MB (10GB) for sbcl

# --array options
#   1,3,4 -> run sub-experiments 1, 3, and 4
#   3-5,9 -> run sub-experiments 3, 4, 5, and 9

# $ bash slurm/run.sh test test-paper-hydra 1,4,5 1 10 02:00:00 8G 10000 exp-name

# experiment equality checker
# $ bash slurm/run.sh train exp-equality 1-11 1 3 00:08:00 8G 10000 exp-equality

# $ bash slurm/run.sh train cle3-size 1-22 1 10 24:00:00 12G 12000 cle3-size
# $ bash slurm/run.sh test test-cle3-size 1-8,11-18,21-22 1 1 06:00:00 10G 10000 cle3-size
# $ bash slurm/run.sh test test-cle3-size-change 1-4 1 1 06:00:00 10G 10000 cle3-size-change