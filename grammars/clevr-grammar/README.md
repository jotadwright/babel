# The CLEVR Grammar

This repository contains the CLEVR Grammar; a grammar operationalised using [Fluid Construction Grammar](https://www.fcg-net.org) for the [CLEVR dataset](https://cs.stanford.edu/people/jcjohns/clevr/). The CLEVR Grammar and its evaluation are described in the paper:

[**Computational Construction Grammar for Visual Question Answering**]() [Jens Nevens](), [Paul Van Eecke]() and [Katrien Beuls]()

The CLEVR Grammar can successfully _comprehend_ (i.e. going from an utterance to a semantic network) and _formulate_ (i.e. going from a semantic network to an utterance) all of the 1M questions of the CLEVR dataset. The grammar was designed based on the [question templates](https://github.com/facebookresearch/clevr-dataset-gen).

## Project Structure

The repository is structured as follows. 

 - The file `fcg-utils.lisp` specifies a de-render mode tailored for the CLEVR Grammar.
 - The file `grammar.lisp` defines the FCG construction inventory. This is bound to the global variable `*CLEVR*`. Here, you can also alter the configurations for the grammar.
 - The file `lex-and-morph.lisp` contains functions for automagically creating lexical and morphological constructions, using metadata from the CLEVR dataset.
 - The file `config.lisp` will make sure that when the `clevr-grammar` package is loaded the lex and morph constructions will be generated and stored in `*CLEVR*`.
 - The files `nominal.lisp`, `query.lisp`, `relate.lisp`, `count.lisp` and `exist.lisp` contain constructions that are re-usable across question types.
 	- `nominal.lisp` contains constructions for creating nominal groups, used for the objects in the CLEVR questions.
 	- `query.lisp` contains constructions used by questions ending on a query.
 	- `relate.lisp` contains constructions for creating prepositional phrases in different question types.
 	- `count.lisp` contains constructions used by questions ending on a count operation.
 	- `exist.lisp` contains constructions used by questions ending an on exist operation.
 - The remainder of the files in the repository all have a filename that is directly linked to a particular question type. Each of these files contains a number of constructions that are specific for that particular question type.

## How to use

To use the CLEVR Grammar, you need to have the [Babel framework](emergent-languages.org) installed. Installation and configuration instructions can be found [on the Babel website](emergent-languages.org/installation). Babel contains all the necessary dependencies to run the CLEVR Grammar. 

Once Babel is up and running, place this repository somewhere within the Babel folder, e.g. `babel/grammars/clevr-grammar`. 

Open the file `start.lisp` and evaluate the first three lines:

```lisp
(ql:quickload :clevr-grammar)
(in-package :clevr-grammar)
(activate-monitor trace-fcg)
```

Next, you can choose any of the example sentences and evaluate them in comprehension or formulation. To see the CLEVR Grammar in action, open a browser on `localhost:8000`. 





