# CLEVR Learning Experiment

An tutor-learner experiment for learning the CLEVR grammar.

## To Do's

 - In some cases, it appears that NIL is added as a unit in an item-based cxn. Bug is hard to reproduce, but must be removed. In which repair does it occur?

 - In some cases, words like 'OF' or 'IS' are extracted by the holophrase -> item-based repairs. This should not occur.

 - A command line interface to run an experiment would be nice

 - Design a script that evaluates the learner's grammar after a series of games, e.g. run 100 questions and check if their meaning is a) found and b) identical to the ground truth

 - In sentences like "How big is X", it finds the lexical cxn 'big', but here it means something different.. How to handle this? FOR NOW; we eliminate these questions do we don't have to deal with it.

 - When parsing fails, it could have created multiple branches in FCG, e.g. permutations of applying lexical cxns. However, depending on the state of the grammar, this is not always the case. When multiple branches occur, which one to take? Now, the longest one is chosen.
