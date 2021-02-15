# CLEVR Learning Experiment

An tutor-learner experiment for learning the CLEVR grammar.

## To Do's

 - In some cases, it appears that NIL is added as a unit in an item-based cxn. Bug is hard to reproduce, but must be removed. Where does it occur?

 - In some cases, words like 'OF' or 'IS' are extracted by the holophrase -> item-based repairs. This should not occur. FOR NOW, these are kept in a global variable list in order to avoid.

 - A command line interface to run an experiment would be nice

 - Design a script that evaluates the learner's grammar after a series of games, e.g. run 100 questions and check if their meaning is a) found and b) identical to the ground truth.

 - In sentences like "How big is X", it finds the lexical cxn 'big', but here it means something different.. How to handle this? FOR NOW; we eliminate these questions do we don't have to deal with it.

 - When parsing succeeds using multiple cxns, but interpretation fails (wrong answer), why not try to compose a program using subsets of the applied cxns instead of making a new holophrase directly? Give priority to lexical cxns? Or sort on score?

 - Additional repairs that generalise starting from item-based cxns

 - Repair when X lexical cxns applied and item-based with Y slots with Y < X?
