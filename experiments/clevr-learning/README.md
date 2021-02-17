# CLEVR Learning Experiment

An tutor-learner experiment for learning the CLEVR grammar.

## Temporary solutions

 - In some cases, words like 'OF' or 'IS' are extracted by the holophrase -> item-based repairs. 
   This should not occur. FOR NOW, these are kept in a global variable list in order to avoid them.

 - In sentences like "How big is X", it finds the lexical cxn 'big', but here it means something different.
   How to handle this? FOR NOW; we eliminate these questions do we don't have to deal with it.

## Evaluation

 - Design a script that evaluates the learner's grammar after a series of games, 
   e.g. run 100 questions and check if their meaning is a) found and b) identical to the ground truth.

 - Run coherence measures on resulting type hierarchy? 

 - Experiment with removing cxns vs. punishing cxns?
	-> set punishment high (e.g. 0.3) such that incorrect cxns are removed quickly

## Technical issues

 - Find a solution against hopeless composer searches?
	-> with irl node-test, for every solution, we check every other solution to remove duplicates
	-> is this really necessary? When will there be duplicate solutions?
	-> without this, composer still takes quite some time when an incorrect partial meaning is provided.
	-> Actually, it is evaluating large programs with many open variables that takes a long time
	-> For the moment, drastic and domain specific node tests have been added. These speed up things significantly!

 - th links now start at 0.1 and only get rewarded
	-> is it useful to punish th links?
	-> increased weights are nowhere to be found!
	-> the function update-th-links is called, but new weights to not appear in the web interface

 - Where do competiting cxns arise?
	When a lex is created using holophrase->item-based and the same lex is recognized later, the lex->item-based will always be triggered. The only way for competing lex cxns to appear is when the holophrase->item-based creates the same lex again, but with a different meaning. How often does this happen?
	-> make a monitor for this!
	-> maybe happens more often with more sentences

 - Edge case in comprehension:
	X lexical cxns applied, and an item-based cxn with Y slots and Y < X!
	This will not trigger any repair! However, it would be a clear case
	for the lex->item-based repair. It is clear that the item-based cxn
	that applied was not sufficient, so it can be ignored.
	Maybe this can be tried after all repairs? Or it needs to be checked
	already in the lex->item-based repair.

 - Similar edge case:
	X lexical cxns applied, item-based with X slots and 1 string in root!
	Agent will try to use item-based->lexical repair, but it should know
	that this item-based does not have enough slots. Add this to item-based->lex repair.

## To Do 

 - When parsing succeeds using multiple cxns, but interpretation fails (wrong answer), why not try to compose a program using subsets of the applied cxns instead of making a new holophrase directly?
	-> There is no way of knowing which of the cxns are actually good? Maybe via the score?

 - run longer experiment with 500 sentences to check convergence
 - run experiment with all level 1 sentences (how many games?)
