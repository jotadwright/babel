#!/usr/bin/env python3

import subprocess
import sys
import re
from pathlib import Path

def get_fname(fpath):
    return Path(fpath).stem

def get_extension(filepath):
    return Path(filepath).suffix

def get_staged_files():
    # Get the list of all added, copied, modified, or renamed files
    result = subprocess.run(['git', 'diff', '--cached', '--name-only', '--diff-filter=A'], stdout=subprocess.PIPE, text=True)
    files = result.stdout.splitlines()
    return files

# search for invalid patterns
def has_underscore(fname):
    pattern = re.compile(r'[_]')
    return pattern.search(fname)

def has_special_characters(fname):
    pattern = re.compile(r'[\s!@#$%^&*()+={}\[\]:;"\'<>,?/\\|`]')
    return pattern.search(fname)

def has_capitalised_letters(fname):
    pattern = re.compile(r'[A-Z]')
    return pattern.search(fname)

# check for invalid patterns
def contains_special_characters(fpath):
    fname = get_fname(fpath)
    return has_special_characters(fname)

def contains_underscore(fpath):
    ext = get_extension(fpath)
    fname = get_fname(fpath)
    whitelist = [".py", ".ipynb"]
    if ext not in whitelist:
        # if not python file, check for underscores
        return has_underscore(fname)
    return False

def contains_capitalised_letters(fpath):
    fname = get_fname(fpath)
    whitelist = ["README", "LICENSE", "AUTHORS"]
    if fname not in whitelist:
        # if not whitelisted, check for capital letters
        return has_capitalised_letters(fname)
    return False
        
def check_fname_length(fpath):
    fname = get_fname(fpath)
    return len(fname) > 50

# check for directories or files
def contains_invalid_directories(fpath):
    # loop through the directories of the given path
    base_dirs = Path(fpath).parent.parts
    # for each directory, check against the convention
    for dir_name in base_dirs:
        if has_special_characters(dir_name) or \
            has_underscore(dir_name) or \
            has_capitalised_letters(dir_name):
            return True
    return False

def is_invalid_filename(fpath):
    return contains_special_characters(fpath) or \
        contains_underscore(fpath) or \
        contains_capitalised_letters(fpath) or \
        check_fname_length(fpath)

def main():
    # Get the list of all added, copied, modified, or renamed files
    staged_files = get_staged_files()

    # Initialize a list to hold filenames with disallowed extensions
    invalid_files = set()
    invalid_dirs = set()

    # check each file for file name convention
    for fpath in staged_files:
        if is_invalid_filename(fpath):
            invalid_files.add(fpath)
        if contains_invalid_directories(fpath):
            invalid_dirs.add(fpath)

    # If there are any invalid files, print an error message and exit with a non-zero status
    if invalid_files:
        print("\tError: The following paths contain files that do not adhere to the required naming convention:")
        for fpath in invalid_files:
            print(f"\t - {fpath}")
    
    if invalid_dirs:
        print("\tError: the following paths contain directories that do not adhere to the required naming convention:")
        for fpath in invalid_dirs:
            print(f"\t - {fpath}")
    
    if invalid_files or invalid_dirs:
        print("\n\tTip: naming convention for files and directories:")
        print("\t * only use dashes between words")
        print("\t * only use lower-case letters (except for a README)")
        print("\t * do not use special characters (except for dashes)")
        print("\t * do not use more than 30 characters")
        sys.exit(1)

    # Exit with zero status if no invalid files are found
    sys.exit(0)

if __name__ == '__main__':
    main()