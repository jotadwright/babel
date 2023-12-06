#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --job-name=comprehension_clevr_symbolic
#SBATCH --ntasks=1 
#SBATCH --mem-per-cpu=20gb
#SBATCH --error /user/brussel/101/vsc10168/comprehension-clevr-symbolic/comprehension-clevr-symbolic-%a.err
#SBATCH --output /user/brussel/101/vsc10168/comprehension-clevr-symbolic/comprehension-clevr-symbolic-%a.log
#SBATCH --array=0-149


cd $SLURM_SUBMIT_DIR
export TMPDIR=${TMPDIR/[/-}
export TMPDIR=${TMPDIR/]/}
mkdir -p $TMPDIR

cd $VSC_DATA/Babel3/applications/visual-dialog/evaluation/job_scripts/

module purge
module load SBCL/2.2.1-GCCcore-10.3.0 

START=$SLURM_ARRAY_TASK_ID
END=$SLURM_ARRAY_TASK_ID

sbcl --dynamic-space-size 6000 --load comprehension-clevr-symbolic.lisp --quit \
     start $START end $END \
