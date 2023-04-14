# Duckie Language Learning

## About

The duckie language learning demo shows the process of language acquisition through intention reading and pattern finding in a situated environment. 
An agent is able to learn a grammar and the underlying meaning representation of the questions. 
Thus, an agent does not need a given meaning representation but will learn it through interactions. 

## Usage

In order to run the experiment, you'll need to go to the ```start.lisp``` file, quickload the ```duckie-language-learning``` package, activate the monitors and then you can start the demo by setting the ```*demo*``` parameter and evaluating ```run-interaction```. 

## Configuration
All default values are defined in ```experiment.lisp```.
The set of repairs that is used by the learner agent is defined in ```grammar.lisp```.