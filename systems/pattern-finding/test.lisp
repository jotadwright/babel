(ql:quickload :pattern-finding)
(in-package :pattern-finding)

;; constructions for anti-unification
(def-fcg-constructions pattern-finding-constructions
  :cxn-inventory *pattern-finding-constructions*
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set)
                  (footprints set))
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       ;(:cxn-supplier-mode . :hashed) ;; use hashing
                       ;; for using heuristics
                       (:search-algorithm . :best-first)
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) 
                       (:heuristic-value-mode . :sum-heuristics-and-parent)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network)))

(let* ((observation-form "What color is the cube?")
       (observation-meaning '((get-context ?context)
                              (filter ?set-1 ?context ?shape-1)
                              (bind shape-category ?shape-1 cube)
                              (unique ?object-1 ?set-1)
                              (query ?target ?object-1 ?attribute-1)
                              (bind attribute-category ?attribute-1 color))))
  (destructuring-bind (cxns-to-apply cxns-to-consolidate cats-to-add links-to-add)
      (find-cxns-and-anti-unify `((sequence ,observation-form ,(make-var 'lb) ,(make-var 'rb)))
                                observation-meaning
                                *pattern-finding-constructions*)
    (loop for cxn in cxns-to-apply
          do (add-element (make-html cxn))
          do (add-cxn cxn *pattern-finding-constructions*))
    (loop for cxn in cxns-to-consolidate
          do (add-cxn cxn *pattern-finding-constructions*))
    (add-categories cats-to-add *pattern-finding-constructions*)
    (loop for (a . b) in  links-to-add
          do (add-link a b *pattern-finding-constructions*))))

(let ((observation-form "What color is the ball?")
      (observation-meaning (fresh-variables
                            '((get-context ?context)
                              (filter ?set-1 ?context ?shape-1)
                              (bind shape-category ?shape-1 sphere)
                              (unique ?object-1 ?set-1)
                              (query ?target ?object-1 ?attribute-1)
                              (bind attribute-category ?attribute-1 color)))))
  (find-cxns-and-anti-unify `((sequence ,observation-form ,(make-var 'lb) ,(make-var 'rb)))
                            observation-meaning
                            *pattern-finding-constructions*))

(let ((observation-form "What size is the ball?")
      (observation-meaning '((get-context ?context)
                             (filter ?set-1 ?context ?shape-1)
                             (bind shape-category ?shape-1 sphere)
                             (unique ?object-1 ?set-1)
                             (query ?target ?object-1 ?attribute-1)
                             (bind attribute-category ?attribute-1 color))))
  (find-cxns-and-anti-unify `((sequence ,observation-form 0 ,(length observation-form)))
                            observation-meaning
                            '<-
                            *pattern-finding-constructions*))