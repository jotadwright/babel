(in-package :pn)

;; ############################################################################
;; Web monitors
;; ----------------------------------------------------------------------------

(defun predicate-network->html (predicate-network)
  `((div)
    ,(predicate-network->svg predicate-network)))

(defun show-anti-unification-results-on-web-interface (pattern source anti-unification-results)
  (add-element '((h1) "Anti-unification"))
  (add-element '((hr)))
  (add-element '((h2) "Pattern network:"))
  (add-element (predicate-network->html pattern))
  (add-element '((h2) "Source network:"))
  (add-element (predicate-network->html source))
  (add-element '((hr)))
  (loop for (resulting-pattern
             resulting-pattern-bindings
             resulting-source-bindings
             cost
             predicates-to-remove) in (sort anti-unification-results #'< :key #'fourth)
        for i from 1
        do (add-element`((h2) ,(format nil "Solution ~a" i)))
        (add-element '((h3) "Resulting pattern:"))
        (add-element (predicate-network->html resulting-pattern))
        (add-element '((h3) "Pattern bindings:"))
        (add-element (html-pprint resulting-pattern-bindings))
        (add-element '((h3) "Source bindings:"))
        (add-element (html-pprint resulting-source-bindings))
        (add-element '((h3) "Cost:"))
        (add-element (html-pprint cost))
        (add-element '((h3) "Removed predicates:"))
        (add-element (if predicates-to-remove
                       (predicate-network->html predicates-to-remove)
                       (html-pprint nil)))
        (add-element '((hr)))))
        