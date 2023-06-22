;;;; grammar.lisp

(in-package :cgl)

(def-fcg-constructions clevr-grammar
    :feature-types ((args set-of-predicates)
                    (form set-of-predicates)
                    (meaning set-of-predicates)
                    (subunits set)
                    (superunits set)
                    (footprints set))
    :fcg-configurations ((:de-render-mode . :de-render-string-meets-precedes-within-3) ;; special de-render mode: precedes within N
                         (:render-mode . :generate-and-test) ;; using the new renderer
                         (:form-predicates meets precedes)
                         (:node-tests :check-duplicate :connected-structure-for-morph :restrict-nr-of-nodes)
                         (:parse-goal-tests :no-applicable-cxns
                                            :connected-semantic-network
                                            :connected-structure ;; !!! also :connected-structure in comprehension
                                            :no-strings-in-root) 
                         (:production-goal-tests :no-applicable-cxns
                                                 :connected-structure
                                                 :no-meaning-in-root)
                         
                         ;; For heuristic search with seq2seq:
                         (:cxn-supplier-mode . :hashed+seq2seq-heuristic)
                         (:priority-mode . :seq2seq-heuristic-additive)
                         (:seq2seq-endpoint . "http://localhost:8888/next-cxn")
                         (:seq2seq-model-comprehension . "clevr_comprehension_model")
                         (:seq2seq-model-formulation . "clevr_formulation_model")
                         (:seq2seq-rpn-fn . clevr-meaning->rpn)
                         
                         ;; For guiding search:
                         (:cxn-sets-with-sequential-application hashed-lex hashed-morph)
                         (:node-expansion-mode . :multiple-cxns)
                         (:queue-mode . :greedy-best-first)
                         (:max-nr-of-nodes . 10000)
                         (:hash-mode . :hash-string-meaning-lex-id))
    :visualization-configurations ((:show-constructional-dependencies . nil)
                                   (:hide-features . nil)
                                   ;(:hide-features . (footprints superunits))
                                   (:with-search-debug-data . t))
    :hierarchy-features (subunits)
    :hashed t
    :cxn-inventory *CLEVR*
    (generate-lexical-constructions *CLEVR*)
    (generate-morphological-constructions *CLEVR*))

;; This is to be able to call comprehend and formulate without specifying the cxn-inventory
(setf *fcg-constructions* *CLEVR*)
