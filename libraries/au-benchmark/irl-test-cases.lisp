
(ql:quickload :au-benchmark)
(in-package :au-benchmark)

;; IRL test cases:

;; Substitution (same category in bind statement)
(print-anti-unification-results
 (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
  ;; are there any red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?cube)
    (bind shape-category ?cube cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (exist ?target ?set-2))
  ;; are there any red spheres?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 sphere)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (exist ?t ?s2))
  :mode :shared-variables))


;; Substitution (different category in bind statement)
(print-anti-unification-results
 (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
  ;; are there any red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (exist ?target ?set-2))
  ;; are there any small cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind size-category ?b2 small)
    (exist ?t ?s2))
  :mode :shared-bindings))

;; => sometimes a bad solution is returned, because only 1-swaps are possible
;;    and because shared bindings may be too sparse. All initial omega values
;;    are zero, which causes a random A-pair to be added as the first one. A
;;    bad choice here can lead to a bad branch in the search which cannot be
;;    repaired by 1-swaps...
;; => bugfix in select-B-pair-C-pair is an improvement, but does not solve the
;;    issue completely. There are still cases where a 1-swap does not suffice
;;    for finding the optimal solution!
;; => using :mode :shared-variables is less sparse and always gives the best
;;    solution for these examples. However, for benchmark cases, the shared
;;    bindings seems to work better (tbc by running the entire benchmark ofc..)

;; Substitution (different final predicate)
(print-anti-unification-results
 (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
  ;; are there any red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (exist ?target ?set-2))
  ;; how many red cubes are there?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (count ?t ?s2))
  :mode :shared-bindings))

;; Substitution (network with cycle)
(print-anti-unification-results
 (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
  ;; how many cubes or spheres?
  '((get-context ?context)
    (filter ?cube-set ?context ?cube)
    (bind shape-category ?cube cube)
    (filter ?sphere-set ?context ?sphere)
    (bind shape-category ?sphere sphere)
    (union ?set ?cube-set ?sphere-set)
    (count ?target ?set))
  ;; how many cubes or cylinders?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?c ?b2)
    (bind shape-category ?b2 cylinder)
    (union ?s3 ?s1 ?s2)
    (count ?t ?s3))
  :mode :shared-bindings))

;; Different order
(print-anti-unification-results
 (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
  ;; how many red large cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?size-1)
    (bind size-category ?size-1 large)
    (filter ?set-3 ?set-2 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-3))
  ;; how many large red cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3))
  :mode :shared-bindings))

;; Deletion
(print-anti-unification-results
 (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
  ;; how many red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-2))
  ;; how many large red cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3))
  :mode :shared-bindings))

;; Addition
(print-anti-unification-results
 (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
  ;; how many large red cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3))
  ;; how many red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-2))
  :mode :shared-bindings))

;; Generalise over previous generalisation
(print-anti-unification-results
 (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
  ;; how many X cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?X)
    (count ?target ?set-3))
  ;; how many X spheres?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 sphere)
    (filter ?s2 ?s1 ?Y)
    (count ?t ?s2))
  :mode :shared-bindings))

;; not minimal difference
(print-anti-unification-results
 (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
  ;; what color is the large cube?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?size-1)
    (bind size-category ?size-1 large)
    (unique ?object-1 ?set-2)
    (query ?target ?object-1 ?attribute-1)
    (bind attribute-category ?attribute-1 color))
  ;; what color is the small blue sphere?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 sphere)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 blue)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 small)
    (unique ?o1 ?s3)
    (query ?t ?o1 ?b4)
    (bind attribute-category ?b4 color))
  :mode :shared-bindings))