#!/bin/bash

# scripts not be run by user, but by slurm/run.sh

# load atools
module purge
module load atools/1.5.1-GCCcore-12.3.0

# read input data from csv
source <(aenv --data $VSC_SCRATCH/babel/experiments/crs-conventionality/batch/config/train/$name.csv --sniff 32768)

# load sbcl
module purge
module load SBCL/2.5.3-GCCcore-13.3.0

# run script
sbcl --dynamic-space-size $space --load $VSC_SCRATCH/babel/experiments/crs-conventionality/batch/run.lisp \
    exp-name $exp_name \
    nr-of-series $nr_of_series \
    nr-of-interactions $nr_of_interactions \
    dataset $dataset \
    datasplit $datasplit \
    nr-of-entities-in-world $nr_of_entities_in_world \
    nr-of-agents-in-population $nr_of_agents_in_population \
    nr-of-entities-in-scene $nr_of_entities_in_scene \
    alignment-strategy $alignment_strategy \
    learning-strategy $learning_strategy \
    learning-rate $learning_rate \
    neighbor-q-value-lr $neighbor_q_value_lr \
    determine-interacting-agents-mode $determine_interacting_agents_mode \
    determine-scene-entities-mode $determine_scene_entities_mode \
    determine-topic-mode $determine_topic_mode \
    seed $seed \
    exp-top-dir $exp_top_dir \
    introduce-agents-after-interaction $introduce_agents_after_interaction \
    nr-of-agents-to-introduce $nr_of_agents_to_introduce \