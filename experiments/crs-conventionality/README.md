# Conventionality in Emergent Languages

This is the codebase for the paper 'Linguistic Conventionality Benefits the Robustness, Learnability and Cognitive Efficiency of Emergent Languages'

## Installation & Dependencies

### Prerequisites

This project requires

- [SBCL](http://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/)
- [Babel](https://gitlab.ai.vub.ac.be/ehai/babel)

Luckily, an [installation guide for Babel, SBCL and Quicklisp](https://emergent-languages.org/wiki/docs/installation/installation.html) is available.


### Set up package

The package `:crs-conventionality` has some dependencies managed by Quicklisp, ensure they are installed by running:

```lisp
(ql:quickload :crs-conventionality)
```

### Setting up a PRNG seed bank

In order to run the batch concept emergence game experiments, you need to setup a PNRG seed bank file by executing:

```lisp
(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)
(generate-seeds 100)
```

## Running the experiments

### Demo experiments

Demo experiments can be found at `scripts/demo.lisp`. These experiments can be run interactively in Lispworks, showing interactions between autonomous agents, or with live plotting of the evolutionary dynamics of the experiments.

### Naming game experiments

The naming game experiments can be found at `scripts/naming-game.lisp`. These experiments are more computationally heavy than the demo experiments and should be run in SBCL. These include the following experiments as described in the paper:

- Naming game base experiments (Fig. 1)
- Naming game noise experiments (Fig. 3)
- Naming game population turnover experiments (Fig. 5)
- Naming game learner agent experiments (Fig. 7)
- Naming game vocabulary limit experiments (Fig. 9)

### Concept emergence game experiments

These experiments must be run on a High-Performance Cluster with SLURM. The scripts for running the experiments are found in `batch/slurm/` and the configuration files in `batch/config`. 
1. To run these experiments, first navigate to the `batch/` folder and compile the package.

    ```bash
    cd batch
    bash slurm/compile.sh
    ```
2. Each experimental configuration can then be run in the following way. Please adjust wall-time and memory as needed.
    - Concept emergence base experiments (Fig. 2)
    ```bash
    bash slurm/run.sh train cle-base 1,2 1-10 12:00:00 10G 10000 cle-base-1
    ```
    - Concept emergence noise experiments (Fig. 4)
    ```bash
    bash slurm/run.sh train cle-noise 1-6 1-10 12:00:00 10G 10000 cle-noise-1
    ```
    - Concept emergence population turnover experiments (Fig. 6)
    ```bash
    bash slurm/run.sh train cle-population-turnover 1,2 1-10 12:00:00 10G 10000 cle-population-turnover-1
    ```
    - Concept emergence learner agent experiments (Fig. 8)
    ```bash
    bash slurm/run.sh train cle-learner-agent 1,2 1-10 12:00:00 10G 10000 cle-learner-agent-1
    ```
    - Concept emergence vocabulary limit experiments (Fig. 10)
    ```bash
    bash slurm/run.sh train cle-vocabulary-limit 1,2 1-10 12:00:00 10G 10000 cle-vocabulary-limit-1
    ```