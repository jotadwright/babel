#!/bin/bash

# see README.md for usage

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

mkdir -p "/user/brussel/101/vsc10156/concept-emergence2/batch/slurm/logs/${1}/${8}"

for i in $(expand_range "$4");
do
    epath="/user/brussel/101/vsc10156/concept-emergence2/batch/slurm/logs/${1}/${8}/${2}_%a_seed${i}_%A_e.txt"
    opath="/user/brussel/101/vsc10156/concept-emergence2/batch/slurm/logs/${1}/${8}/${2}_%a_seed${i}_%A_o.txt"
    sbatch --error $epath --output $opath --job-name "${8}-seed${i}-${1}" --array $3 --time $5 --mem $6  --ntasks 1 --cpus-per-task 1 --export=seed=$i,name=$2,space=$7,exp_top_dir=$8 slurm/$1.sh
done