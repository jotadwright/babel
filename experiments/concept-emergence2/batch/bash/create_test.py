import argparse
import csv
import os

parser = argparse.ArgumentParser()
parser.add_argument("--csv", type=str)
parser.add_argument("--seed", type=int)
parser.add_argument("--exp_top_dir", type=str)

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

sbcl --dynamic-space-size 16000 --load test.lisp \\
    exp-name {exp_name} \\
    nr-of-interactions {nr_of_interactions} \\
    dataset {dataset} \\
    dataset-split {dataset_split} \\
    available-channels "{available_channels}" \\
    scene-sampling {scene_sampling} \\
    topic-sampling {topic_sampling} \\
    seed {seed} \\
    exp-top-dir {exp_top_dir}
"""


def create_bash_script(row):
    return template.format(**row)


def main(input_file, output_dir, exp_fname, seed, exp_top_dir):
    os.makedirs(output_dir, exist_ok=True)
    with open(input_file, "r") as csv_file:
        csv_reader = csv.DictReader(csv_file)

        for idx, row in enumerate(csv_reader, start=1):
            row["seed"] = seed
            row["exp_top_dir"] = exp_top_dir
            script_content = create_bash_script(row)
            with open(f"{output_dir}/{exp_fname}_{idx}.sh", "w") as script_file:
                script_file.write(script_content)


if __name__ == "__main__":
    args = parser.parse_args()
    exp_fname = args.csv
    input_csv_file = (
        f"data-test/{exp_fname}.csv"  # Replace with your input CSV file name
    )
    output_directory = "bash/scripts"  # Replace with your desired output directory

    main(input_csv_file, output_directory, exp_fname, args.seed, args.exp_top_dir)
