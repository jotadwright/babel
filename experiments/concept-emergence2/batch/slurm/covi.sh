#!/bin/bash

# scripts not be run by user, but by slurm/run.sh

# load atools
module purge
module load atools/1.5.1-GCCcore-12.3.0

# read input data from csv
source <(aenv --data $VSC_HOME/concept-emergence2/batch/config/train/$name.csv --sniff 32768)

# load sbcl
module purge
module load SBCL/2.5.3-GCCcore-13.3.0
module load binutils/2.42-GCCcore-13.3.0 # required for distributions package

# run script
sbcl --dynamic-space-size $space --load $VSC_HOME/concept-emergence2/batch/covi.lisp \
    exp-name $exp_name \
    nr-of-series $nr_of_series \
    nr-of-interactions $nr_of_interactions \
    population-size $population_size \
    dataset-loader $dataset_loader \
    min-context-size $min_context_size \
    max-context-size $max_context_size \
    dataset "($dataset)" \
    dataset-view $dataset_view \
    dataset-split $dataset_split \
    feature-set "($feature_set)" \
    disable-features $disable_features \
    amount-disabled-features $amount_disabled_features \
    sensor-noise $sensor_noise \
    sensor-std $sensor_std \
    observation-noise $observation_noise \
    observation-std $observation_std \
    scene-sampling $scene_sampling \
    topic-sampling $topic_sampling \
    align $align \
    entrenchment-incf $entrenchment_incf \
    entrenchment-decf $entrenchment_decf \
    entrenchment-li $entrenchment_li \
    weight-update-strategy $weight_update_strategy \
    initial-weight $initial_weight \
    weight-incf $weight_incf \
    weight-decf $weight_decf \
    weighted-distribution-distance $weighted_distribution_distance \
    switch-condition $switch_condition \
    switch-conditions-after-n-interactions $switch_conditions_after_n_interactions \
    stage-parameters "$stage_parameters" \
    coherence-perspective $coherence_perspective \
    seed $seed \
    exp-top-dir $exp_top_dir