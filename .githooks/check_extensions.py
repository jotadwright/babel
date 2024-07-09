#!/usr/bin/env python3

import subprocess
import sys

def get_staged_files():
    result = subprocess.run(['git', 'diff', '--cached', '--name-only', '--diff-filter=A'], stdout=subprocess.PIPE, text=True)
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
        print("Error: the following files have disallowed extensions:")
        for file in invalid_files:
            print(f" - {file}")
        print("Please remove or rename these files before committing.")
        sys.exit(1)

    # Exit with zero status if no invalid files are found
    sys.exit(0)

if __name__ == '__main__':
    main(sys.argv[1:-1])