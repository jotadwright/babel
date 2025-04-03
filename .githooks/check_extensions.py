#!/usr/bin/env python3

import argparse
import subprocess
import sys

def get_staged_files():
    result = subprocess.run(['git', 'diff', '--cached', '--name-only', '--diff-filter=ACMR'], stdout=subprocess.PIPE, text=True)
    files = result.stdout.splitlines()
    return files

def main(whitelist):
    # Get the list of all added, copied, modified, or renamed files
    staged_files = get_staged_files()

    # Initialize a list to hold filenames with disallowed extensions
    invalid_files = []

    # Check each file for disallowed extensions
    for file in staged_files:
        if '.' in file:  # Ensure file has an extension
            extension = file.split('.')[-1]
            if extension not in whitelist:
                invalid_files.append(file)

    # If there are any invalid files, print an error message and exit with a non-zero status
    if invalid_files:
        print("\tError: the following files have disallowed extensions:")
        for file in invalid_files:
            print(f"\t - {file} [.{file.split('.')[-1]}]")
        print(f"\n\tTip: only commit files with the following extensions:")
        for ext in whitelist:
            print(f"\t * .{ext}")
        sys.exit(1)

    # Exit with zero status if no invalid files are found
    sys.exit(0)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--whitelist', nargs='*')
    # NOTE: hacky way to handle 'pre-commit' argument passing
    # pre-commit appends all staged files in this whitelist args
    # therefore we add a second --blacklist argument which enables us to cut those off
    parser.add_argument('--blacklist', nargs='*')
    args = parser.parse_args()
    main(args.whitelist)