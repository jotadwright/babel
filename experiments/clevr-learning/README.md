# CLEVR Learning Experiment

An tutor-learner experiment for learning the CLEVR grammar.

## To Do's

 - In some cases, words like 'OF' or 'IS' are extracted by the holophrase -> item-based repairs. This should not occur. FOR NOW, these are kept in a global variable list in order to avoid.

 - In sentences like "How big is X", it finds the lexical cxn 'big', but here it means something different.. How to handle this? FOR NOW; we eliminate these questions do we don't have to deal with it.

 - Design a script that evaluates the learner's grammar after a series of games, e.g. run 100 questions and check if their meaning is a) found and b) identical to the ground truth.

 - Additional repairs that generalise starting from item-based cxns

 - Repair when X lexical cxns applied and item-based with Y slots with Y < X?
