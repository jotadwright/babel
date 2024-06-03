#!/bin/bash

#SBATCH --job-name=kswap-grid-search
#SBATCH --time=20:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=4G
#SBATCH --mail-user=jens@ai.vub.ac.be
#SBATCH --mail-type=ALL
#SBATCH --output /user/brussel/101/vsc10168/logs/kswap-gridsearch-%a-out.log
#SBATCH --error /user/brussel/101/vsc10168/logs/kswap-gridsearch-%a-err.log
#SBATCH --array=1-770

module purge
module load SBCL/2.4.1-GCCcore-12.3.0
module load atools/1.5.1-GCCcore-12.3.0

cd $VSC_DATA/quicklisp/local-projects/k-swap-anti-unification/code/

source <(aenv --data ./scripts/parameters.csv --id $SLURM_ARRAY_TASK_ID)

alog --state start
sbcl --dynamic-space-size 2000 --load run-benchmark-cli.lisp --quit \
	-i "./data/${file}" \
	-d "./.tmp/job-${SLURM_ARRAY_TASK_ID}/" \
	-a exhaustive_injective -a exhaustive \
	-a kswap -a kswap_decoupling \
	-a baseline_injective -a baseline \
	-k $k -W $W -V $V -o $scope -p 10 -t 60
alog --state end --exit $?
