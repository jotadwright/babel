#!/bin/bash
#SBATCH --time=24:0:0
#SBATCH --job-name=evaluation_clevr_symbolic
#SBATCH --ntasks=1 
#SBATCH --mem-per-cpu=20gb
#SBATCH --error /user/brussel/102/vsc10279/evaluation-clevr-symbolic/evaluation-clevr-symbolic_%a.log
#SBATCH --output /user/brussel/102/vsc10279/evaluation-clevr-symbolic/evaluation-clevr-symbolic_%a.log
#SBATCH --array=0-149


cd $SLURM_SUBMIT_DIR
export TMPDIR=${TMPDIR/[/-}
export TMPDIR=${TMPDIR/]/}
mkdir -p $TMPDIR

cd $VSC_DATA/ehai-babel/applications/visual-dialog/job_scripts

module purge
module load SBCL/2.2.1-GCCcore-10.3.0 

START=$SLURM_ARRAY_TASK_ID
END=$SLURM_ARRAY_TASK_ID

sbcl --dynamic-space-size 6000 --load evaluation-clevr-symbolic.lisp --quit \
    start $START \
    end $END \
