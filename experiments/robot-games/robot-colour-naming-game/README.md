# The Grounded Colour Naming Game Experiment

This repository contains the Grounded Colour Naming Game experiment, a didactic example implementation of a language game using the [Babel toolkit](https://emergent-languages.org). Both the experiment and the toolkit are described in the following paper:

[Nevens J., Van Eecke P. and Beuls K. (2019). **A Practical Guide to Studying Emergent Communication through Grounded Language Games**, AISB19, Falmouth, 2019.]()

Additionally, the experiment is accompanied by an [online web demonstration](https://emergent-languages.org/demos/aisb19).

## Getting Started

Assuming you have the Babel toolkit installed, go to the file `run-experiment.lisp`. In this file, you will find everything you need to run the Grounded Colour Naming Game experiment. Execute the first few lines of the file to load the system, go into the correct package and activate the web monitors. Now, open a browser window at [http://localhost:8000](). This should display an empty white page with the word _reset_ at the top.

To run the experiment, first evaluate the expressions creating the configuration and the experiment objects. Next, run a single interaction by evaluating `run-interaction` or run a series of interactions by evaluating `run-series`. The web interface should display information about each interaction.

When executing `run-experiments`, you can run a batch of experiments and collect data. The data will be stored in a subfolder of the experiment. When the batch is finished running, this data can be visualised using `create-graph-for-single-strategy`.

## Project Structure

The main interaction script is defined in `interaction-script.lisp`. The experiment is structured such that each level of the semiotic cycle is implemented in a separate file:

 * You will find the functions `embody`, `agent-observe-world`, `choose-topic`, `point-and-observe` and `agent-nod` in the `sensori-motor-level.lisp`.
 * The functions `conceptualise` and `interpret` are defined in `conceptual-level.lisp`
 * The file `language-level.lisp` contains `produce-utterance`, `pass-utterance` and `comprehend-utterance`.
 * At the end of the interaction, both agents align their lexicon. The alignment procedure can be found in `alignment.lisp`. 

The remainder of the project contains files for defining classes and configurations (`classes.lisp`), monitors for collecting data (`monitors.lisp`) and creating visualisations for the web interface (`web-monitors.lisp`). 

## Contact

This repository is maintained by the [EHAI group](https://ehai.ai.vub.ac.be) of the [VUB AI Lab](https://ai.vub.ac.be). If you have any questions, please contact <ehai@ai.vub.ac.be>.
