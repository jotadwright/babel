#!/bin/bash
#SBATCH --time=120:00:00
#SBATCH	--ntasks=1
#SBATCH --job-name=mid_august
#SBATCH --error $VSC_HOME/scratch/Projects/babel/experiments/concept-emergence2/logging/hydra/logs/mid_august_%a.err
#SBATCH --output $VSC_HOME/scratch/Projects/babel/experiments/concept-emergence2/logging/hydra/logs/mid_august_%a.log
#SBATCH --array=0-24

# move to dir
cd $VSC_HOME/scratch/Projects/babel/experiments/concept-emergence2/hydra/

# load atools
module purge
module load atools/1.5.1-GCCcore-11.2.0

# start logging
alog --state start

# read input data from csv
source <(aenv --data $VSC_HOME/scratch/Projects/babel/experiments/concept-emergence2/hydra/mid-august.csv --sniff 4096)

# load sbcl
module purge
module load SBCL/2.2.1-GCCcore-10.3.0 

# run script
sbcl --dynamic-space-size 8000 --load run.lisp \
    exp-name $exp_name \
    population-size $population_size \
    dataset $dataset \
    dataset-split $dataset_split \
    available-channels $available_channels \
    disable-channels $disable_channels \
    amount-of-disabled-channels $amount_of_disabled_channels \
    sensor-noise $sensor_noise \
    sensor-std $sensor_std \
    observation-noise $observation_noise \
    observation-std $observation_std \
    scene-sampling $scene_sampling \
    topic-sampling $topic_sampling \
    similarity-threshold $similarity_threshold \
    align $align \
    entrenchment-incf $entrenchment_incf \
    entrenchment-decf $entrenchment_decf \
    entrenchment-li $entrenchment_li \
    trash-concepts $trash_concepts \
    weight-update-strategy $weight_update_strategy \
    initial-weight $initial_weight \
    weight-incf $weight_incf \
    weight-decf $weight_decf \
    switch-condition $switch_condition \
    switch-conditions-after-n-interactions $switch_conditions_after_n_interactions \
    stage-parameters $stage_parameters

# stop logging
module purge
module load atools/1.5.1-GCCcore-11.2.0
alog --state end