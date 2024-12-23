# K-swap Anti-Unification

This repository contains code to design and benchmark anti-unification algorithms over sets of predicates.

## Project Structure

 - Utils: this directory contains various utility functions for manipulating bindings, lists and sets, symbols and strings, etc. Most of these functions are taken over from the Babel repository (https://emergent-languages.org). As such, they fall under Babel's Apache license. These functions have been copied here such that this repository can function independently from Babel.
 - `classes.lisp`: this file contains CLOS objects that are needed throughout the repository
 - `cost.lisp`: this file contains different methods to compute the cost of an anti-unification result
 - `anti-unify-predicate-sequence.lisp`: implementation of the sequential anti-unification of terms, from Peter Flach's book Simply Logical, chapter 9. This algorithm is used at the end of every anti-unification algorithm that is implemented in this repository. In fact, the different algorithms make an alignment and the sequential anti-unification algorithm is then used to construct the generalisation, the delta's and the bindings lists.
 - Algorithms: this directory contains various anti-unification algorithms
     - lcg: this directory contains anti-unification algorithms for the longest common generalisation, where a generalisation is an injective mapping between the provided sets of predicates. This directory contains an exhaustive algorithm, a baseline algorithm, and an implementation of k-swap as presented by Yernaux and Vanhoof (2019, 2022).
     - msg: this directory contains anti-unification algorithms for the most specific generalisation, where a generalisation is a bijective mapping with respect to some variabilisation function Theta. This directory contains an exhaustive algorithm, a baseline algorithm, and other algorithms in development. 
 - Benchmark: this directory contains code related to benchmarking the algorithms in terms of cost and runtime. 
 - `cli.lisp`: this file defines the command line interface for the repository
