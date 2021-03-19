
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; use this file to run experiments (possibly on the cluster)

(run-experiments '(
                   (5k_questions+30k_games+smart_tutor+decf_04
                    ((:question-sample-method . :first)
                     (:questions-per-challenge . 5000)
                     (:scenes-per-question . 20)
                     (:alignment-strategy . :minimal-holophrases+lateral-inhibition)
                     (:composer-strategy . :store-past-scenes)
                     (:hide-type-hierarchy . t)
                     (:confidence-threshold . 1.1)
                     (:export-interval . 5000)
                     (:cxn-decf-score . 0.4)
                     (:tutor-mode . :smart)))
                   )
                 :number-of-interactions 30000
                 :number-of-series 1
                 :monitors (list ;; success
                                 "export-communicative-success" 
                                 ;; overall lexicon size
                                 "export-lexicon-size"
                                 ;; lexical size per type
                                 "plot-num-cxns-per-type"
                                 ;; overall cxn scores
                                 "export-avg-cxn-score"
                                 ;; cxn scores per type
                                 "plot-cxn-score-per-type"
                                 ;; type of applied cxns
                                 "plot-cxn-usage-per-type"
                                 ;; competition for lexical cxns
                                 "export-lexical-meanings-per-form"
                                 "export-lexical-forms-per-meaning"
                                 ;; nr of item based cxns with nr of slots
                                 "plot-nr-of-slots"
                                 ;; export data
                                 "export-learner-grammar"
                                 ;; print dots
                                 "print-a-dot-for-each-interaction"
                                 ))

