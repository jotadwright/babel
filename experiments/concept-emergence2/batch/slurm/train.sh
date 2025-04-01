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
sbcl --dynamic-space-size $space --load $VSC_HOME/concept-emergence2/batch/run.lisp \
    exp-name $exp_name \
    nr-of-series $nr_of_series \
    nr-of-interactions $nr_of_interactions \
    population-size $population_size \
    world-size $world_size \
    dataset-loader $dataset_loader \
    min-context-size $min_context_size \
    max-context-size $max_context_size \
    dataset "($dataset)" \
    dataset-view $dataset_view \
    dataset-split $dataset_split \
    scene-sampling $scene_sampling \
    topic-sampling $topic_sampling \
    align $align \
    entrenchment-incf $entrenchment_incf \
    entrenchment-decf $entrenchment_decf \
    entrenchment-li $entrenchment_li \
    coherence-perspective $coherence_perspective \
    interacting-agents-strategy $interacting_agents_strategy \
    boltzmann-tau $boltzmann_tau \
    boltzmann-lr $boltzmann_lr \
    network-topology $network_topology \
    local-connectivity $local_connectivity \
    rewiring-probability $rewiring_probability \
    seed $seed \
    exp-top-dir $exp_top_dir