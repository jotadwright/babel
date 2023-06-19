# The CLEVR Grammar

This repository contains the CLEVR Grammar; a grammar operationalised using [Fluid Construction Grammar](https://www.fcg-net.org) for the [CLEVR dataset](https://cs.stanford.edu/people/jcjohns/clevr/). The CLEVR Grammar and its evaluation are described in the paper:

[**Computational Construction Grammar for Visual Question Answering**]()
[Jens Nevens](), [Paul Van Eecke]() and [Katrien Beuls]()

The CLEVR Grammar can successfully _comprehend_ (from utterance to semantic network) and _formulate_ (from semantic network to utterance) all of the 1M questions of the CLEVR dataset. The grammar was designed based on the [question templates](https://github.com/facebookresearch/clevr-dataset-gen).

## Project Structure

The repository is structured as follows. 

 - fcg-utils.lisp
 - grammar.lisp
 - lex-and-morph.lisp
 - nominal, query, relate, count, exist
 - the rest

## How to use

To use the CLEVR Grammar, you need to have the [Babel framework](emergent-languages.org) installed. Installation and configuration instructions can be found [here](emergent-languages.org/installation). Babel contains all the necessary dependencies to run the CLEVR Grammar. 

Once Babel is up and running, place this repository somewhere within the Babel folder, e.g. `babel/grammars/clevr-grammar`. 

Open the file `start.lisp`. Evaluate the first three lines. Next, you can choose any of the example sentences and evaluate them in comprehension or formulation. To see the CLEVR Grammar in action, open a browser on `localhost:8000`. 





