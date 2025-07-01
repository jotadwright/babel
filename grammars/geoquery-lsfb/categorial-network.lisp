(in-package :geoquery-lsfb-grammar)

(let ((categorial-network (categorial-network *geoquery-lsfb*)))
  (add-categories
   '(states-next-to-?x\(?x\)-cat
     states-next-to-?x-cat
     highest-points-of-?x\(?x\)-cat
     highest-points-of-?x-cat
     cities-in-?x\(?x\)-cat
     largest-of-?x\(?x\)-cat
     cities-in-?x-cat
     answer-?x\(?x\)-cat
     largest-of-?x-cat
     x-population-of-?y\(?y\)-cat
     x-population-of-?y\(?x\)-cat
     largest-cat
     x-population-of-?y-cat
     sum-cat
     answer-?x\(?x\)-cat-2
     state-cat
     cityid-cat
     stateid-cat
     states-next-to-cat
     x-has-most-?y\(?x\)-cat
     x-has-most-?y\(?y\)-cat
     x-has-most-?y-cat
     highest-place-of-?x-cat
     highest-place-of-?x\(?x\)-cat
     state-with-capital-?x-cat
     state-with-capital-?x\(?x\)-cat
     rivers-traversing-?x\(?x\)-cat
     rivers-traversing-?x-cat)
   categorial-network)
  ;(add-link 'states-next_to-reference-cat 'mississippi-cat categorial-network)
  (add-link 'states-next-to-?x-cat 'highest-points-of-?x\(?x\)-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat 'highest-points-of-?x-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat 'states-next-to-?x-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat 'x-population-of-?y-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat-2 'x-population-of-?y-cat categorial-network)
  (add-link 'states-next-to-?x-cat 'cities-in-?x\(?x\)-cat categorial-network)
  (add-link 'cities-in-?x-cat 'largest-of-?x\(?x\)-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat 'largest-of-?x-cat categorial-network)
  (add-link 'x-population-of-?y\(?x\)-cat 'largest-cat categorial-network)
  (add-link 'x-population-of-?y\(?x\)-cat 'sum-cat categorial-network)
  (add-link 'x-population-of-?y\(?y\)-cat 'states-next-to-?x-cat categorial-network)
  (add-link 'x-has-most-?y\(?x\)-cat 'state-cat categorial-network)
  (add-link 'states-next-to-cat 'x-has-most-?y\(?y\)-cat categorial-network)
  (add-link 'answer-?x\(?x\)-cat 'x-has-most-?y-cat categorial-network)
  (add-link 'highest-place-of-?x\(?x\)-cat 'states-next-to-?x-cat categorial-network)
  (add-link 'highest-place-of-?x-cat 'answer-?x\(?x\)-cat categorial-network)
  (add-link 'state-with-capital-?x-cat 'states-next-to-?x\(?x\)-cat categorial-network)
  (add-link 'states-next-to-?x\(?x\)-cat 'stateid-cat categorial-network)
  (add-link 'state-with-capital-?x\(?x\)-cat 'cityid-cat categorial-network)
  (add-link 'states-next-to-?x-cat 'rivers-traversing-?x\(?x\)-cat categorial-network)
  (add-link 'rivers-traversing-?x-cat 'answer-?x\(?x\)-cat categorial-network))
