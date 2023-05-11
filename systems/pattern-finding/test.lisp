(ql:quickload :pattern-finding)
(in-package :pattern-finding)

;; constructions for anti-unification
(def-fcg-constructions pattern-finding-constructions
  :cxn-inventory *pattern-finding-constructions*
  :feature-types ((sequences sequence)
                  (args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set))
  :fcg-configurations ((:cxn-supplier-mode . :hashed-labeled-positive-scores)
                       (:hash-mode . :hash-string-meaning)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours))

)

(let ((observation-form "What color is the cube?")
      (observation-meaning '((get-context ?context)
                             (filter ?set-1 ?context ?shape-1)
                             (bind shape-category ?shape-1 cube)
                             (unique ?object-1 ?set-1)
                             (query ?target ?object-1 ?attribute-1)
                             (bind attribute-category ?attribute-1 color))))
  (find-cxns-and-anti-unify `((sequence ,observation-form 0 ,(length observation-form)))
                            observation-meaning
                            '<-
                            *pattern-finding-constructions*))


;; QUESTIONS:

;; - Include cost of string alignment in string anti unification cost?
;; - How to do hashing with sequence predicates in the form?