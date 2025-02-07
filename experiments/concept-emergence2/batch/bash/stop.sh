#!/bin/bash

expand_range() {
    input="$1"
    result=()

    # Convert commas to newlines, then process each item
    IFS=',' read -ra parts <<< "$input"
    for part in "${parts[@]}"; do
        if [[ "$part" =~ ^([0-9]+)-([0-9]+)$ ]]; then
            # It's a range, expand it
            start=${BASH_REMATCH[1]}
            end=${BASH_REMATCH[2]}
            for ((i = start; i <= end; i++)); do
                result+=("$i")
            done
        else
            # It's an individual number
            result+=("$part")
        fi
    done

    # Return the expanded numbers as a space-separated string
    echo "${result[@]}"
}

for i in $(expand_range "$2");
do
    #screen -S $1_$i -X quit
    echo "Stopped screen $1_$i"
done