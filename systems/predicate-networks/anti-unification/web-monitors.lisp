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

(defun draw-anti-unification-test-case (network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)
  (add-element '((h1) "Anti-Unification"))
  (add-element '((hr)))
  (add-element '((h2) "Input:"))
  (add-element
   `((table)
     ((tr)
      ((td) ((b) "Network 1"))
      ((td) ((b) "Network 2")))
     ((tr)
      ((td) ,(html-pprint network-1 :max-width 100)
       ,(predicate-network->svg network-1))
      ((td) ,(html-pprint network-2 :max-width 100)
       ,(predicate-network->svg network-2)))))
  (add-element '((h2) "Output:"))
  (add-element
   `((table)
     ((tr)
      ((td) ((b) "MSG")))
     ((tr)
      ((td) ,(html-pprint msg :max-width 100)
       ,(predicate-network->svg msg)))))
  (add-element
   `((table)
     ((tr)
      ((td) ((b) "Bindings 1"))
      ((td) ((b) "Bindings 2")))
     ((tr)
      ((td) ,(html-pprint bindings-1 :max-width 100))
      ((td) ,(html-pprint bindings-2 :max-width 100)))
     ((tr)
      ((td) ((b) "Delta 1"))
      ((td) ((b) "Delta 2")))
     ((tr)
      ((td) ,(html-pprint delta-1 :max-width 100)
       ,(predicate-network->svg delta-1))
      ((td) ,(html-pprint delta-2 :max-width 100)
       ,(predicate-network->svg delta-2))))))


        