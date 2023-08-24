#!/bin/bash
mkdir -p $VSC_HOME/concept-emergence2/batch/slurm/logs/$1
sbatch <<EOT
#!/bin/bash
#SBATCH --job-name=$1
#SBATCH --error=$VSC_HOME/concept-emergence2/batch/slurm/logs/$1/test_$1_%a_e.txt
#SBATCH --output=$VSC_HOME/concept-emergence2/batch/slurm/logs/$1/test_$1_%a_o.txt
#SBATCH --time=$4
#SBATCH	--ntasks=1
#SBATCH --cpus-per-task=5
#SBATCH --mem=$3
#SBATCH --array=$2

# load atools
module purge
module load atools/1.5.1-GCCcore-11.2.0

# read input data from csv
source <(aenv --data $VSC_HOME/concept-emergence2/batch/data/$1.csv --sniff 4096)

# load sbcl
module purge
module load SBCL/2.2.1-GCCcore-10.3.0

# run script
sbcl --dynamic-space-size 16000 --load $VSC_HOME/concept-emergence2/batch/test.lisp \
    exp-name $exp_name \
    nr-of-interactions $nr_of_interactions \
    dataset $dataset \
    dataset-split $dataset_split \
    available-channels "$available_channels" \
    scene-sampling $scene_sampling \
    topic-sampling $topic_sampling
EOT