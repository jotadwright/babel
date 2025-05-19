# Concept Grammar Learning Experiment

## Installation

### Setting up a PRNG seed bank

First setup a PNRG seed bank file by executing:

```lisp
(ql:quickload :utils)
(in-package :utils)
(generate-seeds 100)
```

### Data requirements

All data is expected to be stored under `~/Corpora/`.

For the CLEVR questions

- Download from NextCloud `~/Files/EHAI/corpora/CLEVR-like datasets/CLEVR-intention-reading-data.zip`
- Locally unzip and place at `~/Corpora/CLEVR-intention-reading-data/`

For the CLEVR scenes

- Download from NextCloud `~/Files/EHAI/corpora/concept-emergence/concept-grammar-learning.zip` (! TODO !)
- Locally unzip and place the two folders under:
  - `/Corpora/concept-emergence2/split-by-scenes/clevr-extracted`
  - `/Corpora/concept-emergence2/split-by-scenes/clevr-simulated`

## Notes that should definitely be looked at

### Update 10/05/2023

- Disclaimer: the naming convention for particular functions/configs in `systems/` has changed considerably.
  For example, `th` was renamed to `categorial`.
  Therefore, we recommend to revert to an earlier commit where `systems/` was in sync.

### Technical issues

- (DONE) Find a solution against hopeless composer searches?
  -> with irl node-test, for every solution, we check every other solution to remove duplicates
  -> is this really necessary? When will there be duplicate solutions?
  -> without this, composer still takes quite some time when an incorrect partial meaning is provided.
  -> Actually, it is evaluating large programs with many open variables that takes a long time
  -> For the moment, drastic and domain specific node tests have been added. These speed up things significantly!
  -> However, these will probably no longer work when entering the level 3 questions!

### To Do

- Do something with multiple composer solutions? Make hypotheses?
- Do something with multiple comprehension branches? Make hypotheses?

### Assumptions

There might be some hidden assumptions present in the current state of the experiment.
Here is an attempt to list them.

- In all counting questions, examples with the answer '0' are left out. These are just carte blanche to learn something useless.

- When composing a new program, there could be multiple solutions. These solutions are ordered according to the default :chunk-evaluation-result-scoring-mode which is :chunk-and-binding-score-with-few-duplicates. When the composer strategy is being used (e.g. check past scenes), the chunk evaluation results are provided in this order. Otherwise, when the composer strategy is not active (because it was the first time encountering this utterance), the first solution is returned. This would indicate that the default scoring mode is appropriate. But is it? The composer could rank its solutions based on how good the chunks involved are (these are all scored uniformly for the moment) and how good the cxns involved are (these do have scores reflecting their success). IN SHORT: should there be a chunk scoring mechanism? And how would it work?
