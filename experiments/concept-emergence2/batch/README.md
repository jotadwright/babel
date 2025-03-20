# A note on running experiments in batch

The `concept-emergence2/batch/` directory contains scripts for running large-scale experiments in the terminal.
In general, all experiments can be executed either locally using Bash or on a SLURM-managed cluster.
Of course, experiments can also be run interactively with LispWorks/Emacs using the scripts in `concept-emergence2/scripts/`.

First, navigate to the `batch/` directory.

```bash
cd babel/experiments/concept-emergence2/batch
```

Importantly, all following scripts assume that commands are run inside the `batch/` directory.

## Creating an experimental configuration

The project contains a lot of configurable parameters.
To this end, we store experimental configurations in `batch/config`.

To create a new experiment, the first step will always to create a new configuration.
In this project, sets of experiments will first be stored into a single group inside a `csv`.

- Configurations for training are stored in `batch/config/train/`.
- Configurations for training are stored in `batch/config/test/`.

For example, `batch/config/train/cle3-smcl2.csv` groups all experimental configurations for playing the concept learning game on 37 different tabular datasets.

- Inside the `csv`, every row corresponds to a single experiment.
- Each row gets an unique id and a unique name. The id is used when submitting jobs on a cluster (see parameter `$3` in `Submitting jobs`)
- The unique name is used a a directory name to store all runs of that experiment.
- Every experiment is run multiple times so that we can measure the statistics of different runs.
- At runtime a unique directory name is generated for every run.

In summary, when an experiment is completed the data will be logged using the following structure:

```
logging/
   experiment-root-directory/ <- `cle3-smcl2`
      experiment-1/           <- `smcl-1`
         run-1/               <- `2025-01-13_17h12m56s-seed2-20336`
         run-2/               <- `2025-01-13_17h12m58s-seed3-35932`
         ...
      experiment-2/           <- `smcl-2`
         ...
      ...
```

## Running the Code

### Option 1: Local Execution via terminal

To run experiments locally, directly call the `batch/run.lisp` with sbcl:

```bash
sbcl --dynamic-space-size 16000 --non-interactive --load batch/run.lisp
    exp-name experiment-7
    nr-of-series 1
    nr-of-interactions 5000
    population-size 10
    ...
```

Additionally, we also provide a number of scripts to automate the generation of such commands based on stored experimental configurations (in `batch/config/train` or `batch/config/test`)

All scripts to run the code locally in the terminal are found in `batch/bash/`.

To run the code locally in the terminal, execute the following steps:

1. Generate the necessary Bash scripts using Python:

   - For training:
     ```bash
     python bash/create_train --csv paper-hydra
     ```
   - For testing:
     ```bash
     python bash/create_test --csv paper-hydra
     ```
     The `--csv` argument specifies the name of a file in `batch/config/train` or `batch/config/test`. This will generate multiple Bash scripts in `batch/bash/scripts/...`.

2. Execute the generated Bash scripts:
   ```bash
   bash bash/scripts/paper-hydra_1.sh 1,3,5 experiment-1
   ```
   This will run the first configuration detailed in `batch/config/train/paper-hydra.csv`.
   It will run it on seeds 1, 3 and 5. Finally, all three seeded runs will be logged at `logging/train/experiment-1/`.

### Option 2: Remote execution on a HPC cluster via SLURM

**NOTE** : all scripts were developed and tested only on VUB's Hydra HPC cluster.

To run experiments on a cluster, use the `batch/slurm/` directory.

1. **Compilation Step**: The first time you run an experiment (before running any other scripts) execute once the **`compile.sh`** script:

   ```bash
   bash compile.sh
   ```

2. **Submitting Jobs**: - Submit experiments to the clusters with **`run.sh`**.

   Example usage:

   ```bash
   bash slurm/run.sh train paper-hydra 1,3,4 1-5,9 08:00:00 10G 10000 experiment-7
   ```

   - `$1`: Mode (`train` or `test`)
   - `$2`: Name of the CSV file with configuration (found in `batch/config/train/`)
   - `$3`: Atools-like arrays, specifies which sub-experiments to run (e.g. `1,3,4`)
   - `$4`: Atools-like arrays, specifies which seeds to run (e.g. `1-5,9`)
   - `$6`: Wall-time allocation (e.g. `08:00:00` for 8 hours, on Hydra limit is `120:00:00`)
   - `$7`: Memory requirement (e.g. `10G` for 10GB, amount depends on dataset used)
   - `$8`: Dynamic-space-size for SBCL in megabytes (e.g. `10000` MB or 10GB, match with memory requirement)
   - `$9`: Experiment name (in this case, experiment would log to `logging/train/experiment-7`)

   **Atools array options:**

   - details: https://atools.readthedocs.io/en/latest/job_arrays/
   - `1,3,4` → Run sub-experiments 1, 3, and 4
   - `1-5,9` → Run sub-experiments 1, 2, 3, 4, 5, and 9
