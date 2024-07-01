#!/bin/bash

#SBATCH --job-name=kswap-grid-search
#SBATCH --time=14:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=10G
#SBATCH --mail-user=jens@ai.vub.ac.be
#SBATCH --mail-type=ALL
#SBATCH --output /home/jnevens/logs/kswap-job-%a-out.log
#SBATCH --error /home/jnevens/logs/kswap-job-%a-err.log
#SBATCH --array=1-770

cd /home/jnevens/quicklisp/local-projects/k-swap-anti-unification/code/

module purge
module use /srv/cluster/software/x86-64-v3/easybuild/2021b/modules/all/
module load atools/1.5.1-GCCcore-11.2.0
source <(aenv --data ./scripts/parameters.csv --id $SLURM_ARRAY_TASK_ID)

sbcl --dynamic-space-size 2000 --load run-benchmark-cli.lisp --quit \
	-i "./data/${file}" \
	-a exhaustive_injective -a exhaustive \
	-a kswap -a kswap_decoupling \
	-a baseline_injective -a baseline \
	-k $k -W $W -V $V -o $scope -p 10 -t 600
