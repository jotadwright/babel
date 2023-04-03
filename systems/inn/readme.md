# Integrative Narrative Networks
<b>Remi van Trijp</b>
<br>Sony Computer Science Laboratory - Paris
<br>[remi.vantrijp@sony.com](mailto:remi.vantrijp@sony.com)
![](docs/inn.png)
<hr>

The `:inn` package offers a framework for building networks and visualizing and interactively modifying them using [vis.js](https://visjs.org/) in Babel's web interface. While the package can be used for many kinds of networks, it is finetuned for supporting <b>integrative narrative networks</b>:

* <b>Integrative:</b> The networks may include nodes and edges that represent information coming from various knowledge sources (e.g. the human user, knowledge graphs and ontologies) and information processing tasks (e.g. semantic parsing, constructional language processing, sensorimotor perception). 
* <b>Narrative:</b> The networks are built on the notion of "narrative questions" that a human or AI system may ask during the sense-making process. The network comes to "narrative closure" if the main narrative question has been answered.
* <b>Networks:</b> The choice for representing the understanding process as a network of narrative questions (and answers) helps to visualize the interaction between many different components, and can be exploited for measuring the degree of understanding that a system has about a certain topic or task.

## Conceptual Foundations

To be completed. In the meantime, please read the following paper that formed the inspiration for the `:inn` package.

* Steels, Luc and Lara Verheyen and Remi van Trijp (2022). [An Experiment in Measuring Understanding](https://ceur-ws.org/Vol-3322/short6.pdf). In: Lise Stork, Katrien Beuls and Luc Steels (eds.), Proceedings of the Workshop on Semantic Techniques for Narrative-Based Understanding
co-located with 31st International Joint Conference on Artificial Intelligence and the 25th European Conference on Artificial Intelligence (IJCAI-ECAI 2022), pp. 36-42. Vienna: CEUR Workshop Proceedings.

## Technical Documentation

If it is your first time using the `:inn` package, please check the file `examples.lisp` for a hands-on introduction to integrative narrative networks.

### Network Class ###

#### <i>class</i>               **integrative-narrative-network**
<hr>
**Description:**         Base class for integrative narrative networks.<br>
**Parent class:**       `graph-utils:graph`
<p align="center">
	<img src="docs/inn-class.png" width="200"</img>
</p>
**Example:**             `(make-instance 'integrative-narrative-networks)`<br>

### Nodes and Edges ###

The networks of the `:inn` package are built on top of the `:graph-utils` library, which defines nodes and edges as defstructs. This makes it more difficult to create custom kinds of nodes because the `:include` option of a defstruct only accepts one `parent' structure.

#### <i>defstruct</i>               **inn-node**
<hr>
**Description:**         Basic "class" for nodes.<br>
**Includes:**              `graph-utils:node`<br>












## Known Issues
* Open narrative questions not always change color automatically when necessary
* Storing and restoring networks is currently incompatible with OpenCCL.