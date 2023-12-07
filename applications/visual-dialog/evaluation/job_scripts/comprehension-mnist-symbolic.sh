#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --job-name=comprehension_mnist_symbolic
#SBATCH --ntasks=1 
#SBATCH --mem-per-cpu=20gb
#SBATCH --error /user/brussel/101/vsc10168/logs/comprehension-mnist-symbolic-%a.err
#SBATCH --output /user/brussel/101/vsc10168/logs/comprehension-mnist-symbolic-%a.log
#SBATCH --array=0-299


cd $SLURM_SUBMIT_DIR
export TMPDIR=${TMPDIR/[/-}
export TMPDIR=${TMPDIR/]/}
mkdir -p $TMPDIR

cd $VSC_DATA/Babel3/applications/visual-dialog/evaluation/job_scripts/

module purge
module load SBCL/2.3.11-GCCcore-11.3.0 

START=$SLURM_ARRAY_TASK_ID
END=$SLURM_ARRAY_TASK_ID

sbcl --dynamic-space-size 6000 --load comprehension-mnist-symbolic.lisp --quit \
     start $START end $END \
