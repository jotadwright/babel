#!/usr/bin/env python

import argparse
import json
import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


parser = argparse.ArgumentParser()
parser.add_argument('--input-json',
                    default='coherence-data.json',
                    help='The json file containing the coherence data')
parser.add_argument('--output-dir',
                    default='./coherence-plots/',
                    help='The output directory')


def main(args):
    # Check if the input_json is actually a json file
    name, ext = os.path.splitext(args.input_json)
    if ext != '.json':
        raise Exception(f'Expected a .json file as input, got {args.input_json}')
    # Create the output dir if it does not exist
    os.makedirs(args.output_dir, exist_ok=True)
    # Read the data into a DataFrame
    with open(args.input_json, 'r') as f:
        data = json.load(f)
    df = pd.DataFrame(data)
    # Each row is a single barplot
    for i in range(len(df)):
        row = df.iloc[i, :].dropna().sort_values(ascending=False)
        row_name = row.name
        ax = sns.barplot(x=row.index.to_numpy(),
                         y=row, palette='crest')
        # Add a title
        ax.set_title(row_name)
        # Set the ylim to [0, 1]
        ax.set_ylim(0, 1)
        # If many xlabels, rotate them
        if len(row) > 6:
            ax.tick_params(axis='x', labelrotation=45)
        else:
            ax.tick_params(axis='x', labelrotation=0)
        # Set ylabel
        ax.set_ylabel('Weighted Cosine Similarity')
        # Export
        outfile = os.path.join(args.output_dir, f'{row_name}.pdf')
        plt.savefig(outfile, format='pdf', bbox_inches='tight')
        plt.clf()


if __name__ == '__main__':
    args = parser.parse_args()
    main(args)
