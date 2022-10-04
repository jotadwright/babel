#!/bin/bash

#SBATCH --job-name=evaluation_mnist_hybrid
#SBATCH --output=/home/laraverheyen/logging/evaluation-mnist-hybrid/evaluation-mnist-hybrid_%a.log
#SBATCH --error=/home/laraverheyen/logging/evaluation-mnist-hybrid/evaluation-mnist-hybrid_%a.err
#SBATCH --time=00-02:00:00
#SBATCH --ntasks-per-node=3
#SBATCH --mem=20G
#SBATCH --array=0-99

cd ~/neural-modules/

module purge

. ~/miniconda3/etc/profile.d/conda.sh
conda activate neural-modules

PORT=$((9000 + $SLURM_ARRAY_TASK_ID))
REDIS_PORT=$((7000 + $SLURM_ARRAY_TASK_ID))
START=$SLURM_ARRAY_TASK_ID
END=$SLURM_ARRAY_TASK_ID

redis-server --port $REDIS_PORT &
sleep 60

PYTHONPATH=$PWD:$PYTHONPATH python scripts/server/server.py  \
    --experiment "mnist_dialog"  \
    --images_dir "data/mnist_dialog/images"  \
    --server_port $PORT \
    --redis_port $REDIS_PORT & \

sleep 300

cd ../babel/applications/visual-dialog/evaluation/job_scripts/

sbcl --dynamic-space-size 6000 --load evaluation-mnist-hybrid.lisp --quit \
    start $START \
    end $END \
    port $PORT \
