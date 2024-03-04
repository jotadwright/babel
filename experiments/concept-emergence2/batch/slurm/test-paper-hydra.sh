#!/bin/bash
#SBATCH --job-name=test-paper
#SBATCH --error=/user/brussel/101/vsc10156/concept-emergence2/batch/slurm/logs/test-paper-hydra_%a_e.txt
#SBATCH --output=/user/brussel/101/vsc10156/concept-emergence2/batch/slurm/logs/test-paper-hydra_%a_o.txt
#SBATCH --time=12:00:00
#SBATCH	--ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=6G
#SBATCH --array=1-20

# load atools
module purge
module load atools/1.5.1-GCCcore-11.2.0

# read input data from csv
source <(aenv --data $VSC_HOME/concept-emergence2/batch/data-test/test-paper-hydra.csv --sniff 4096)

# load sbcl
module purge
module load SBCL/2.2.1-GCCcore-10.3.0

# run script
sbcl --dynamic-space-size 60000 --load $VSC_HOME/concept-emergence2/batch/test.lisp \
    exp-name $exp_name \
    nr-of-interactions $nr_of_interactions \
    dataset $dataset \
    dataset-split $dataset_split \
    available-channels "$available_channels" \
    scene-sampling $scene_sampling \
    topic-sampling $topic_sampling