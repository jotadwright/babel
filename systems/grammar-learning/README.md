# grammar-learning

Package for learning construction grammars

test-repairs.lisp : testcases for individual repairs

clevr-learning.lisp: main file to learn the CLEVR grammar

# notes about IRL

IRL predicates, cfr prolog predicates: e.g.: sum(3,4,X), in prolog: unification, in irl, these primitives are implemented with individual methods, not necessarily unification (e.g. a machine learning lib to do visual filtering)
the first argument in these predicates it typically the output argument (convention) (in prolog it's the last one)
 e.g.: (some-primitive output input1 input2), with filter the last elem is the prototype

 NB: bind is different from filter and unique! IRL is typed, and bind adds the typing to the objects => slightly different from other predicates e.g. when hashing cxns, bind is using the 4th argument, whereas filter uses the first.

 IRL also has a composer, that creates IRL networks given a certain task that a speaker wants to conceptualise

 usually binds correspond to words and other predicates to phrases (the unique predicate could also be implemented as (bind selector-category ?something unique) in combination with the (bind-selector) predicate.

# TODO

