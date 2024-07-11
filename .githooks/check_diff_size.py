#!/usr/bin/env python3

import subprocess
import sys

def get_diff_size():
    # Get the diff of the staged changes
    diff = subprocess.run(['git', 'diff', '--cached'], capture_output=True, text=True)
    # Calculate the size of the diff
    diff_size_bytes = len(diff.stdout.encode('utf-8'))
    diff_size_kb = diff_size_bytes / 1024
    
    return diff_size_kb

def main(max_kb):
    max_kb = int(max_kb)
    diff_size = get_diff_size()
    if diff_size > max_kb:
        print(f"\tError: The commit diff is too large. Current size is {diff_size:.2f} KB, but maximum allowed is {max_kb:.2f} KB.")
        sys.exit(1)
    else:
        sys.exit(0)

if __name__ == '__main__':
    main(sys.argv[1])