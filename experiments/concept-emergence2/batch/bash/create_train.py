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

sbcl --dynamic-space-size 16000 --load run.lisp \\
    exp-name {exp_name} \\
    nr-of-series {nr_of_series} \\
    nr-of-interactions {nr_of_interactions} \\
    population-size {population_size} \\
    dataset {dataset} \\
    dataset-split {dataset_split} \\
    available-channels "{available_channels}" \\
    disable-channels {disable_channels} \\
    amount-disabled-channels {amount_disabled_channels} \\
    sensor-noise {sensor_noise} \\
    sensor-std {sensor_std} \\
    observation-noise {observation_noise} \\
    observation-std {observation_std} \\
    scene-sampling {scene_sampling} \\
    topic-sampling {topic_sampling} \\
    similarity-threshold {similarity_threshold} \\
    align {align} \\
    entrenchment-incf {entrenchment_incf} \\
    entrenchment-decf {entrenchment_decf} \\
    entrenchment-li {entrenchment_li} \\
    trash-threshold {trash_threshold} \\
    slow-threshold {slow_threshold} \\
    conceptualisation-heuristics {conceptualisation_heuristics} \\
    weight-update-strategy {weight_update_strategy} \\
    initial-weight {initial_weight} \\
    weight-incf {weight_incf} \\
    weight-decf {weight_decf} \\
    switch-condition {switch_condition} \\
    switch-conditions-after-n-interactions {switch_conditions_after_n_interactions} \\
    stage-parameters "{stage_parameters}"
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
    input_csv_file = (
        f"data-train/{exp_fname}.csv"  # Replace with your input CSV file name
    )
    output_directory = "bash/scripts"  # Replace with your desired output directory

    main(input_csv_file, output_directory, exp_fname)
