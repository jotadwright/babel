#!/bin/bash

# load atools
module purge
module load atools/1.5.1-GCCcore-12.3.0

# read input data from csv
source <(aenv --data $VSC_HOME/concept-emergence2/batch/data-test/$name.csv --sniff 4096)

# load sbcl
module purge
module load SBCL/2.4.1-GCCcore-12.3.0

# run script
sbcl --dynamic-space-size $space --load $VSC_HOME/concept-emergence2/batch/test.lisp \
    exp-name $exp_name \
    nr-of-interactions $nr_of_interactions \
    dataset-loader $dataset_loader \\
    dataset $dataset \
    dataset-split $dataset_split \
    feature-set $feature_set \
    scene-sampling $scene_sampling \
    topic-sampling $topic_sampling \
    seed $seed \
    exp-top-dir $exp_top_dir