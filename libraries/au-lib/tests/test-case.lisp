(in-package :au-benchmark)

;; what color is the cube?
(defparameter *pattern*
  '((get-context ?context)
    (filter ?shape2 ?context-1 ?shape1)
    (bind shape-category ?shape1 cube)
    (unique ?object-1 ?shape2)
    (query ?target ?object-1 ?attribute-1)
    (bind attribute-category ?attribute-1 color)))

;; what color is the green cube
(defparameter *source*
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 green)
    (unique ?o1 ?s2)
    (query ?t ?o1 ?b4)
    (bind attribute-category ?b4 color)))

;; 2 AU results
;; 1 where get-context -- filter is cut
;; 1 where filter -- count is cut
(defparameter *au-results*
  (au-benchmark.msg.exhaustive:anti-unify-predicate-networks
   (shuffle *pattern*) (shuffle *source*)))

(anti-unification-cost (pattern (first *au-results*))
                       (source (first *au-results*))
                       (generalisation (first *au-results*))
                       (pattern-delta (first *au-results*))
                       (source-delta (first *au-results*))
                       (pattern-bindings (first *au-results*))
                       (source-bindings (first *au-results*))
                       :pattern-finding)
;; cost 6

(anti-unification-cost (pattern (second *au-results*))
                       (source (second *au-results*))
                       (generalisation (second *au-results*))
                       (pattern-delta (second *au-results*))
                       (source-delta (second *au-results*))
                       (pattern-bindings (second *au-results*))
                       (source-bindings (second *au-results*))
                       :pattern-finding)
;; cost 7

(print-anti-unification-results
 (au-benchmark.msg.exhaustive:anti-unify-predicate-networks
  ;; what color is the x cube?
  '((get-context ?context)
    (filter ?shape2 ?context-1 ?shape1)
    (bind shape-category ?shape1 cube)
    (unique ?object-1 ?shape2)
    (query ?target ?object-1 ?attribute-1)
    (bind attribute-category ?attribute-1 color))
  ;; what color is the small blue cube
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 green)
    (unique ?o1 ?s2)
    (query ?t ?o1 ?b4)
    (bind attribute-category ?b4 color))))


