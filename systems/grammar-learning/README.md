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


## Overview of repairs
### add-th-links
* What is the size of the red cube? ==> nothing->holophrase
* What is the size of the blue cube? ==> holophrase->item-based+lexical+lexical--substitution ==> "what-is-the-size-of-the-?X-cube", blue, red
* What is the size of the yellow cube? ==> repair-item-based-to-lexical ==> new link from "what-is-the-size-of-the-X-cube" to yellow 
* What is the size of the green sphere? ==> nothing->holophrase
* What is the size of the purple sphere? ==> holophrase->item-based+lexical+lexical--substitution ==> what-is-the-size-of-the-?X-sphere, purple, green
* What is the size of the purple cube? ==> item-based-to-lexical ==> a link is added between "what-is-the-size-of-the-?X-sphere" and purple
* What is the size of the yellow sphere? ==> add-th-links ==> there is an indirect link between "what-is-the-size-of-the-?X-sphere" and yellow (via purple), create a direct th link.

### item-based->lexical
* The large gray object is what shape? ==> nothing->holophrase
* The gray object is what shape? ==> holophrase->item-based+lexical+holophrase--deletion ==> the-?X-gray-object-is-what-shape, large, the-gray-object-is-what-shape?
* The small gray object is what shape? ==> item-based->lexical ==> small is learnt, with a link to "the-?X-gray-object-is-what-shape"

### holophrase->item-based+lexical+lexical--substitution
* The large gray object is what shape? ==> nothing->holophrase "the-large-gray-object-is-what-shape"
* The large metallic object is what shape? ==> holophrase->item-based+lexical+lexical--substitution ==> "the-large-?X-object-is-what-shape", metallic, gray

### holophrase->item-based+lexical--addition
* The gray object is what shape? ==> nothing->holophrase "the-gray-object-is-what-shape"
* The large gray object is what shape? ==> holophrase->item-based+lexical--addition ==> "the-?X-gray-object-is-what-shape", large

### holophrase->item-based+lexical+holophrase--deletion
* The large gray object is what shape? ==> nothing->holophrase "the-large-gray-object-is-what-shape"
* The gray object is what shape? ==> holophrase->item-based+lexical+holophrase--deletion ==>  "the-?X-gray-object-is-what-shape", large, "the-gray-object-is-what-shape"

### repair-lexical->item-based-cxn
* The gray object is what shape? ==> nothing->holophrase "the-gray-object-is-what-shape"
* The large gray object is what shape? ==> holophrase->item-based+lexical--addition ==> the-?X-gray-object-is-what-shape, large
* What is the color of the large object? ==> repair-lexical->item-based-cxn ==> fill in "large" and create "what-is-the-color-of-the-?X-object"

### nothing->holophrase
* The gray object is what shape? ==> nothing->holophrase "the-gray-object-is-what-shape"