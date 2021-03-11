# CLEVR Learning Experiment

An tutor-learner experiment for learning the CLEVR grammar.

## Temporary solutions

 - In some cases, words like 'OF' or 'IS' are extracted by the holophrase -> item-based repairs. 
   This should not occur. FOR NOW, these are kept in a global variable list in order to avoid them.

 - In sentences like "How big is X", it finds the lexical cxn 'big', but here it means something different.
   How to handle this? FOR NOW; we eliminate these questions so we don't have to deal with it.

## Evaluation

 - Run coherence measures on resulting type hierarchy? 

## Technical issues

 - (DONE) Find a solution against hopeless composer searches?
	-> with irl node-test, for every solution, we check every other solution to remove duplicates
	-> is this really necessary? When will there be duplicate solutions?
	-> without this, composer still takes quite some time when an incorrect partial meaning is provided.
	-> Actually, it is evaluating large programs with many open variables that takes a long time
	-> For the moment, drastic and domain specific node tests have been added. These speed up things significantly!
	-> However, these will probably no longer work when entering the level 3 questions!

 - th links now start at 0.1 and only get rewarded
	-> is it useful to punish th links?
        -> when cxns get punished and eventually removed, the associated nodes in the th are removed as well

## To Do 

 - Do something with multiple composer solutions? Make hypotheses?
 - Do something with multiple comprehension branches? Make hypotheses?

 - When parsing succeeds using multiple cxns, but interpretation fails (wrong answer), 
   why not try to compose a program using subsets of the applied cxns instead of making a new holophrase directly?
	-> There is no way of knowing which of the cxns are actually good? Maybe via the score?

 - Try out the learner's grammar in formulation. What would be necessary to make the learner also speak during the game? Could the learner actively choose a particular question in a particular scene to fill a knowledge gap?

## Assumptions

There might be some hidden assumptions present in the current state of the experiment.
Here is an attempt to list them.

 - In all counting questions, examples with the answer '0' are left out. These are just carte blanche to learn something useless.

 - It is assumed that the learner agent will always apply the most abstract cxns. This is due to the cxn supplier :ordered-by-label-and-score. The parse order is set to lexical, item-based, holophrase. Consequently, as many lexical cxns as possible will be applied first, necessarily followed by an item-based cxn that can take that many slots. 

 - When faced with a parsing issue (i.e. comprehension yields no solution, but one or more leaf nodes), the agent also has some preference. 
	- If there is only 1 node, take that one. 
	- If one of the leaf nodes is a second-merge-failed, take that one. This will lead straight to the add-th-links repair. 
	- Otherwise, take the node with the most general item-based cxn
	- If no item-based cxn could apply, take the node with the most lexical cxns
	- If no node was chosen by now, take one at random.

 - When composing a new program, there could be multiple solutions. These solutions are ordered according to the default :chunk-evaluation-result-scoring-mode which is :chunk-and-binding-score-with-few-duplicates. When the composer strategy is being used (e.g. check past scenes), the chunk evaluation results are provided in this order. Otherwise, when the composer strategy is not active (because it was the first time encountering this utterance), the first solution is returned. This would indicate that the default scoring mode is appropriate. But is it? The composer could rank its solutions based on how good the chunks involved are (these are all scored uniformly for the moment) and how good the cxns involved are (these do have scores reflecting their success). IN SHORT: should there be a chunk scoring mechanism? And how would it work?
