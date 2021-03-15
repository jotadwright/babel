# IRL v2.0

This repository contains a re-implementation of the Incremental Recruitment Language (IRL). The goal is to make it somewhat easier to use, to have an API that is similar to FCG and to enhance the web interface visualisations. In this document, we describe the most important parts of the re-implementation. Note that IRL v2.0 is still in active development. Thus, anything discussed here could change at any moment.

## Seeing IRL v2.0 in action

For the moment, only the CLEVR primitives make use of IRL v2.0. To see this in action, go to the file `applications/clevr-evaluation/demo.lisp` and run the `evaluate-clevr` function. Make sure to activate the monitors first. 

## Creating an ontology

The ontology contains the semantic entities over which the primitive operations can operate. There are two requirements to create a functional ontology. First, it must be a `blackboard` or a subclass thereof. Second, all semantic entities must be an `entity` or a subclass thereof.

## Defining a _primitive inventory_

New in IRL v2.0 is the primitive inventory. Similar to FCG, it holds the primitive operations and a range of configurations that will influence how these primitives are used when evaluating an IRL program. A primitive inventory can be created through the `def-irl-primitives` macro. The default primitive inventory is stored in the global variable \*irl-primitives\*. Currently, the following configurations are accepted:

 - :goal-tests
 - :node-tests
 - :check-irl-program-before-evaluation
 - :queue-mode
 - :priority-mode
 - :max-search-depth
 - :max-nr-of-nodes

## Defining primitives

As before, primitives are defined using the `defprimitive` macro. Here, a slight change in the notation has taken place, as the body of the primitive requires an additional set of parentheses. These might be removed again in upcoming versions.

## Evaluating IRL programs

To evaluate an IRL program, there is the `evaluate-irl-program` function. In the simplest form, this function takes only two arguments: the IRL program to evaluate and the ontology. Using keyword arguments, you can specify the primitive inventory to use (`:primitive-inventory`), to operate in silent mode (`:silent`) and the number of solutions to return (`:n`). By default, all solutions are returned.

Similar to FCG, we make use of a processor and processor node data structures. The `evaluate-irl-program` function returns both the list of solutions as the list of solution nodes.

## Chunking

There are two ways in which chunks can be created: `create-chunk-from-primitive` and `create-chunk-from-irl-program`. These functions will automatically search for the target variable and the open variables and create a chunk. The former function accepts both the name of the primitive as found in an IRL program, or a primitive object as found in a primitive inventory.

## The composer
