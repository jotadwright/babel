# Guessing games at scale

Codebase for the paper 'Emergent Communication in Continuous Worlds: Self-Organisation of Conceptually Grounded Vocabularies at Scale' by Botoko Ekila et al.

## Installation & Dependencies

### Prerequisites

This project requires

- [SBCL](http://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/beta/)
- [Babel](https://gitlab.ai.vub.ac.be/ehai/babel)

Luckily, an [installation guide for Babel, SBCL and Quicklisp](https://emergent-languages.org/wiki/docs/installation/installation.html) is available.

**IMPORTANT**:

1. Install Babel at `~/Projects/babel`.
2. Create a directory at `~/Corpora`

### Setup babel

After installing the prerequisites, create a file `~/Projects/babel/init-babel-user.lisp`:

```lisp
(in-package :cl-user)

(export '(*babel-corpora* *localhost-user-dir*))

(defparameter *babel-corpora* (babel-pathname :directory '(:up :up "Corpora")))
(defparameter *localhost-user-dir* "http://localhost/~<home_folder>/")
```

### Setup package

The package of this project `:cle` has some dependencies managed by Quicklisp, ensure they are installed by running:

```lisp
(ql:quickload :cle)
```

### Setting up a PRNG seed bank

Afterwards, setup a PNRG seed bank file by executing:

```lisp
(ql:quickload :cle)
(in-package :cle)
(generate-seeds 100)
```

## Running the project

### Data requirements

All data is expected to be stored under `~/Corpora/concept-emergence2/`.

To generate the data required for this project, please refer to the [ehai/conll-tabular-datasets](https://gitlab.ai.vub.ac.be/ehai/conll-tabular-datasets) project.

We provide two ways to load data in.

1. split-by-entities: a `.jsonl` file containing all entities (e.g. for tabular datasets).
2. split-by-scenes: a separate `.jsonl` file for each scene, where scenes are precomputed (e.g. for datasets like CLEVR)

```
~/Corpora/concept-emergens2/
    -feature-sets/
        air.csv
        clevr.csv
        ...
    split-by-entities
        air/
            air-test.jsonl
            air-train.jsonl
        ...
    split-by-scenes
        clevr/
            scenes/
                train/
                    CLEVR_train_0000000.json
                    CLEVR_train_0000001.json
                    ...
                test/
                    CLEVR_test_0000000.json
                    ...
```

### Running a multi-agent experiment

We provide two ways to run an experiment:

- **`scripts/`**: Interactively in a REPL (e.g. LispWorks or SBCL+Emacs+SLIME) for development.
  - see `scripts/demo.lisp` for a small demo.
- **`batch/`**: Non-interactively via batch execution in the terminal using SBCL, either locally or on a SLURM cluster, for running large-scale experiments.
  - see `batch/README.md` for an overview.
