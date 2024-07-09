#!/usr/bin/env python3

import subprocess
import sys
import re

def get_fname(filepath):
    return filepath.split('/')[-1].split('.')[0]

def get_staged_files():
    # Get the list of all added, copied, modified, or renamed files
    result = subprocess.run(['git', 'diff', '--cached', '--name-only', '--diff-filter=ACMR'], stdout=subprocess.PIPE, text=True)
    files = result.stdout.splitlines()
    return files

def check_no_under_or_space(fname):
    # no spaces, special chars, capital letters
    pattern = re.compile(r'[\s!@#$%^&*()+={}\[\]:;"\'<>,.?/\\|`]')
    return pattern.search(fname)

def check_fname_length(fname):
    return len(fname) > 30

def main():
    # Get the list of all added, copied, modified, or renamed files
    staged_files = get_staged_files()

    # Initialize a list to hold filenames with disallowed extensions
    invalid_files = []

    # check each file for file name convention
    for file in staged_files:
        fname = get_fname(file)
        if check_no_under_or_space(fname):
            print("problemo")
            invalid_files.append(file)
        if check_fname_length(fname):
            print("ploblemo")
            invalid_files.append(file)

    # If there are any invalid files, print an error message and exit with a non-zero status
    if invalid_files:
        print("Error: the following files contain [underscores, spaces, spacial characters or capital letters] in their filenames:")
        for file in invalid_files:
            print(f" - {file}")
        print("Please rename these files to use dashes instead.")
        sys.exit(1)

    # Exit with zero status if no invalid files are found
    sys.exit(0)

if __name__ == '__main__':
    main()