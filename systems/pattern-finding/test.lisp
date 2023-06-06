(ql:quickload :pattern-finding)
(in-package :pattern-finding)

(activate-monitor trace-fcg)

(defun fcg-learn (form meaning)
  (add-element '((h1) "FCG Learn:"))
  (add-element `((h3) ,(format nil "Observation form: \"~a\"" form)))
  (add-element '((h3) "Observation meaning:"))
  (add-element `((div) ,(irl-program->svg meaning)))
  (add-element '((hr)))
  (destructuring-bind (cxns-to-apply cxns-to-consolidate cats-to-add links-to-add)
      (find-cxns-and-anti-unify `((sequence ,form ,(make-var 'lb) ,(make-var 'rb)))
                                (fresh-variables meaning)
                                *pattern-finding-constructions*)
    (add-element '((h3) "Cxns to apply:"))
    (loop for cxn in cxns-to-apply
          do (add-element (make-html cxn))
             (add-cxn cxn *pattern-finding-constructions*))
    (add-element '((h3) "Cxns to consolidate:"))
    (loop for cxn in cxns-to-consolidate
          do (add-element (make-html cxn))
             (add-cxn cxn *pattern-finding-constructions*))
    (add-element '((h3) "Categories to add:"))
    (add-element (html-pprint cats-to-add))
    (add-categories cats-to-add *pattern-finding-constructions*)
    (add-element '((h3) "Links to add:"))
    (add-element (html-pprint links-to-add))
    (loop for (a . b) in links-to-add
          do (add-link a b *pattern-finding-constructions*))
    (add-element '((hr)))))

;; constructions for anti-unification
(def-fcg-constructions pattern-finding-constructions
  :cxn-inventory *pattern-finding-constructions*
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set)
                  (footprints set))
  ;; :hashed t
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       ;; (:cxn-supplier-mode . :hashed-labeled-positive-scores)
                       ;; for using heuristics
                       (:search-algorithm . :best-first)
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) 
                       (:heuristic-value-mode . :sum-heuristics-and-parent)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network)))

(fcg-learn "What color is the cube"
           '((get-context ?context)
             (filter ?set-1 ?context ?shape-1)
             (bind shape-category ?shape-1 cube)
             (unique ?object-1 ?set-1)
             (query ?target ?object-1 ?attribute-1)
             (bind attribute-category ?attribute-1 color)))

(fcg-learn "What color is the ball"
           '((get-context ?context)
             (filter ?set-1 ?context ?shape-1)
             (bind shape-category ?shape-1 sphere)
             (unique ?object-1 ?set-1)
             (query ?target ?object-1 ?attribute-1)
             (bind attribute-category ?attribute-1 color)))
;; => what-color-is-the-SLOT-cxn + cube-cxn + ball-cxn

(fcg-learn "What size is the ball"
           '((get-context ?context)
             (filter ?set-1 ?context ?shape-1)
             (bind shape-category ?shape-1 sphere)
             (unique ?object-1 ?set-1)
             (query ?target ?object-1 ?attribute-1)
             (bind attribute-category ?attribute-1 size)))
;; => what-SLOT-is-the-ball-cxn + size-cxn + color-cxn

;; => what-SLOT1-is-the-SLOT2-cxn + size-cxn + color-cxn
;; need partial analysis


