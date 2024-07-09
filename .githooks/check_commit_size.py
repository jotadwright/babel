#!/usr/bin/env python3

import os
import sys
from subprocess import check_output

def get_staged_files():
    result = check_output(['git', 'diff', '--cached', '--name-only']).decode('utf-8')
    return result.strip().split('\n')

def get_file_size(file_path):
    return os.path.getsize(file_path)

def main(max_size_mb):
    try:
        max_size_mb = float(max_size_mb)
    except ValueError:
        print("Error: Max size must be a number.")
        sys.exit(1)
    
    staged_files = get_staged_files()
    total_size = sum(get_file_size(file) for file in staged_files if os.path.isfile(file))
    total_size_mb = total_size / (1024 * 1024)

    # If the total size of changes exceeds the maximum size, print an error message and exit with a non-zero status
    if total_size_mb > max_size_mb:
        print(f"Error: Total size of changes exceeds {max_size_mb} MB. Current size: {total_size_mb:.2f} MB.")
        sys.exit(1)

    # Exit with zero status if no invalid files are found
    sys.exit(0)

if __name__ == '__main__':
    main(sys.argv[1])
