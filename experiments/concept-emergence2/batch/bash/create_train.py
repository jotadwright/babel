import argparse
import csv
import os

parser = argparse.ArgumentParser()
parser.add_argument("--csv", type=str)

# assumes folder structure:
# batch/
#   bash/
#       create.py
#       run.lisp
#       scripts/
#           script_1.sh
#           ...
#           script_x.sh
template = """\
#!/bin/bash

sbcl --dynamic-space-size 16000 --non-interactive --load run.lisp \\
    exp-name {exp_name} \\
    nr-of-series {nr_of_series} \\
    nr-of-interactions {nr_of_interactions} \\
    population-size {population_size} \\
    dataset-loader {dataset_loader} \\
    min-context-size {min_context_size} \\
    max-context-size {max_context_size} \\
    dataset "({dataset})" \\
    dataset-view {dataset_view} \\
    dataset-split {dataset_split} \\
    feature-set "({feature_set})" \\
    disable-features {disable_features} \\
    amount-disabled-features {amount_disabled_features} \\
    sensor-noise {sensor_noise} \\
    sensor-std {sensor_std} \\
    observation-noise {observation_noise} \\
    observation-std {observation_std} \\
    scene-sampling {scene_sampling} \\
    topic-sampling {topic_sampling} \\
    align {align} \\
    entrenchment-incf {entrenchment_incf} \\
    entrenchment-decf {entrenchment_decf} \\
    entrenchment-li {entrenchment_li} \\
    weight-update-strategy {weight_update_strategy} \\
    initial-weight {initial_weight} \\
    weight-incf {weight_incf} \\
    weight-decf {weight_decf} \\
    weighted-distribution-distance {weighted_distribution_distance} \\
    switch-condition {switch_condition} \\
    switch-conditions-after-n-interactions {switch_conditions_after_n_interactions} \\
    stage-parameters "{stage_parameters}" \\
    coherence-perspective {coherence_perspective} \\
    seed ${{1}} \\
    exp-top-dir ${{2}}
"""


def create_bash_script(row):
    return template.format(**row)


def main(input_file, output_dir, exp_fname):
    os.makedirs(output_dir, exist_ok=True)
    with open(input_file, "r") as csv_file:
        csv_reader = csv.DictReader(csv_file)

        for idx, row in enumerate(csv_reader, start=1):
            script_content = create_bash_script(row)
            with open(f"{output_dir}/{exp_fname}_{idx}.sh", "w") as script_file:
                script_file.write(script_content)


if __name__ == "__main__":
    args = parser.parse_args()
    exp_fname = args.csv
    input_csv_file = f"config/train/{exp_fname}.csv"
    output_directory = "bash/scripts"

    main(input_csv_file, output_directory, exp_fname)
