# seq2seq-heuristics

Seq2seq-heuristics is a Babel subsystem for integrating seq2seq heuristics in Fluid Construction Grammar. It loads its functions, methods and classes into the FCG package.

The system provides functionalities for querying seq2seq models (next_cxn) and for using the results in FCG (construction-suppliers and priority-modes).

For using the system, set the following entries in the configuration of your construction inventory:

- A seq2seq compatible construction supplier mode, e.g.:
`(:cxn-supplier-mode . :hashed+seq2seq-heuristic)`
- A seq2seq comptatible priority mode, e.g.:
  `(:priority-mode . :seq2seq-heuristic-additive)`
- The endpoint where the seq2seq model can be queried, e.g.:
`(:seq2seq-endpoint . "http://localhost:8888/next_cxn")`
- The name of the seq2seq model to use in comprehension, e.g.:
`(:seq2seq-model-comprehension . "clevr_comprehension_model")`
- The name of the seq2seq model to use in production, e.g.:
`(:seq2seq-model-formulation . "clevr_formulation_model")`