# Incremental Recruitment Language

This repository contains (a re-implementation of) the Incremental Recruitment Language (IRL).
The goal of this re-implementation is to make it somewhat easier to use, to have an API that is similar to FCG and to enhance the web interface visualisations. 
In this document, I describe the most important changes that are part of this re-implementation.

## Creating an ontology

The ontology contains the semantic entities over which the primitive operations can operate.
There are two requirements to create a functional ontology. 
First, it must be a `blackboard` or a subclass thereof.
Second, all semantic entities must be an `entity` or a subclass thereof.

## Defining a _primitive inventory_

New in IRL is the primitive inventory. 
Similar to FCG, it stores the primitive operations and a range of configurations that will influence how these primitives are used during evaluation. 
A primitive inventory can be created through the `def-irl-primitives` macro. 
The default primitive inventory is stored in the global variable `*irl-primitives*`.
Currently, the following configurations are accepted:

 - :goal-tests (default '(:no-primitives-remaining :all-variables-bound))
 - :node-tests (default '(no-duplicate-solutions))
 - :check-irl-program-before-evaluation (default T)
 - :shuffle-primitives-before-evaluation (default T)
 - :search-algorithm (default :best-first)
 - :heuristics (default '(:nr-of-evaluated-primitives))
 - :heuristic-value-mode (default :sum-heuristics-and-parent)
 - :primitive-supplier-mode (default :simple-queue)
 - :node-expansion-mode (default :full-expansion)

## Defining primitives

As before, primitives are defined using the `defprimitive` macro. 
Here, a slight change in the notation has taken place, as you can specify the primitive inventory in which the current primitive will be stored.
This is done through the keyword `:primitive-inventory`, used at the end of the primitive definition.
If unspecified, the primitive will be stored in the default inventory `*irl-primitives*`.

## Evaluating IRL programs

To evaluate an IRL program, there is the `evaluate-irl-program` function. 
In the simplest form, this function takes only two arguments: the IRL program to evaluate and the ontology. 
Using keyword arguments, you can specify the primitive inventory to use (`:primitive-inventory`), to operate in silent mode (`:silent`) and the number of solutions to return (`:n`). 
By default, all solutions are returned.

Similar to FCG, we make use of a primitive inventory processor (pip) and primitive inventory processor node (pipn) data structures. 
The `evaluate-irl-program` function returns the list of solution bindings, the solution pip nodes and the pip itself.
