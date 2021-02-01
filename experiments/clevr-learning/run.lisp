
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; use this file to run experiments (possibly on the cluster)

(run-experiments '(
                   (test
                    ((:questions-per-challenge . 500)
                     (:alignment-strategy . :minimal-holophrases+lateral-inhibition)
                     (:composer-strategy . :store-past-scenes)
                     (:hide-type-hierarchy . t)))
                   )
                 :number-of-interactions 15000
                 :number-of-series 1
                 :monitors (list ;; success
                                 "export-communicative-success"
                                 ;; lexicon size
                                 "export-lexicon-size"
                                 "export-nr-of-holophrase-cxns"
                                 "export-nr-of-item-based-cxns"
                                 "export-nr-of-lexical-cxns"
                                 ;; cxn scores
                                 "export-avg-cxn-score"
                                 "export-avg-holophrase-cxn-score"
                                 "export-avg-item-based-cxn-score"
                                 "export-avg-lexical-cxn-score"
                                 ;; type of applied cxns
                                 "export-holophrase-cxn-usage"
                                 "export-item-based-cxn-usage"
                                 ;; others
                                 "export-lexicon-change"
                                 "export-confidence-level"
                                 "export-type-hierarchy"
                                 "export-learner-grammar"
                                 ))