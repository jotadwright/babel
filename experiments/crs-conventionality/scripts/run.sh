#!/bin/bash
# Pass directory containing babel
cd $1

# call this file using
# sbatch --error "$VSC_SCRATCH/logs/e.txt" --output "$VSC_SCRATCH/logs/o.txt" --job-name "jamie" --time 01:00:00 --mem 10G  --ntasks 1 --cpus-per-task 1 run.sh $VSC_SCRATCH

# load atools
module purge

# load sbcl
module purge
module load SBCL/2.4.1-GCCcore-12.3.0
module load binutils/2.40-GCCcore-12.3.0 # required for distributions package

# run script
sbcl --dynamic-space-size 10000 --load /babel/experiments/crs-conventionality/scripts/sbcl-run.lisp
