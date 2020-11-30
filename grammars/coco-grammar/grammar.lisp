;;;; grammar.lisp

(in-package :coco-grammar)

(def-fcg-constructions coco-grammar
    :feature-types ((args set-of-predicates)
                    (form set-of-predicates)
                    (meaning set-of-predicates)
                    (subunits set)
                    (superunits set)
                    (footprints set))
    :fcg-configurations ((:de-render-mode . :de-render-string-meets-precedes-within-3) ;; special de-render mode: precedes within N
                         (:render-mode . :generate-and-test) ;; using the new renderer
                         (:form-predicates meets precedes)
                         (:node-tests :check-duplicate :restrict-nr-of-nodes)
                         (:parse-goal-tests :no-applicable-cxns
                                            :connected-semantic-network
                                            :connected-structure
                                            :no-strings-in-root) ;; !!! also :connected-structure in comprehension
                         (:production-goal-tests :no-applicable-cxns
                                                 :connected-structure
                                                 :no-meaning-in-root)
                         ;; For guiding search:
                         (:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
                         (:priority-mode . :nr-of-applied-cxns)                         
                         (:node-expansion-mode . :multiple-cxns)
                         (:queue-mode . :greedy-best-first)
                         (:max-nr-of-nodes . 10000)
                         (:hash-mode . :hash-string-meaning-lex-id))
    :visualization-configurations ((:show-constructional-dependencies . nil)
                                   (:hide-features . (footprints superunits))
                                   (:with-search-debug-data . t))
    :hierarchy-features (subunits)
    :hashed t
    :cxn-inventory *COCO*
    (generate-lex-and-morph-cxns *COCO*))

;; This is to be able to call comprehend and formulate without specifying the cxn-inventory
(setf *fcg-constructions* *COCO*)
