# CLEVR Grammar Learning

## About

The CLEVR grammar learning experiment lets a learner agent learn a bi-directional grammar from scratch from corpus data, consisting of utterances with their IRL meaning representations.



## Usage

In order to run the experiment, you'll need to compile the ```start.lisp``` file and evaluate the ```run-training``` and ```run-evaluation``` functions.

## Configuration
All default values are defined in ```experiment.lisp```.
The set of repairs that is used by the learner agent is defined in ```grammar.lisp``` .  


## Dataset and pre-processing

The CLEVR dataset was filtered and divided into 3 stages, each filtered on a set of primitives. Please see ```data-preprocessor.lisp``` for details.
The path to the filtered datasets needs to be defined in ```experiment.lisp``` before running the experiment. Please contact Jens Nevens if you'd like a copy of the unfiltered data, or Jonas Doumen if you'd like the filtered dataset.
