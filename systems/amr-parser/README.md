# amr-parser

This library facilitates the manipulation of AMR meaning representations (Banarescu et al. 2016) in Common Lisp. Warning: it parses different AMR notations, not natural language!

It exports the following high-level functions:

- `penman->predicates`: takes as input a meaning representation in Penman notation and returs the same meaning representation in predicate notation.

```
(penman->predicates '(w / want-01
						:arg0 (b / boy)
						:arg1 (g / go-01
									:arg0 b)))
									
=> ((WANT-01 W) (BOY B) (GO-01 G) (ARG0 W B) (ARG1 W G) (ARG0 G B))
```

- `predicates->penman`: takes as input a meaning representation in predicate notation and returs the same meaning representation in Penman notation.

```
(predicates->penman '((WANT-01 W) (BOY B) (GO-01 G) (ARG0 W B) (ARG1 W G) (ARG0 G B)))
									
=> '(w / want-01
		:arg0 (b / boy)
		:arg1 (g / go-01
					:arg0 b))
```

It also exports the following more low-level functions:

- `penman->object`: takes a meaning representation in Penman notation and returns a CLOS object of the class `amr-object`, capturing the information contained in it.

- `object->predicates`: takes a CLOS object of the class `amr-object` and returns a meaning representation in predicate notation.

- `predicates->object`: takes a meaning representation in predicate notation and returns a CLOS object of the class `amr-object`.

- `object->penman`: takes a CLOS object of the class `amr-object` and returns a meaning representation in Penman notation.