
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; use this file to run experiments (possibly on the cluster)

(run-experiments '(
                   (cxn-decf-02-15k-games
                    ((:question-sample-method . :first)
                     (:questions-per-challenge . 5000)
                     (:scenes-per-question . 20)
                     (:alignment-strategy . :minimal-holophrases+lateral-inhibition)
                     (:composer-strategy . :store-past-scenes)
                     (:hide-type-hierarchy . t)
                     (:confidence-threshold . 1.1)
                     (:cxn-decf-score . 0.2)
                     (:tutor-mode . :smart)
                     (:tutor-counts-failure-as . 3)))
                   )
                 :number-of-interactions 15000
                 :number-of-series 2
                 :monitors (list ;; success
                                 "export-communicative-success"
                                 ;; overall lexicon size
                                 "export-lexicon-size"
                                 ;; overall cxn scores
                                 "export-avg-cxn-score"
                                 ;; lexical size per type
                                 "plot-lexicon-size-per-type" 
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
                                 "export-type-hierarchy"
                                 ;; print dots
                                 "print-a-dot-for-each-interaction"
                                 ))



(run-experiments '(
                   (tutor-counts-2
                    ((:question-sample-method . :first)
                     (:questions-per-challenge . 5000)
                     (:scenes-per-question . 20)
                     (:alignment-strategy . :minimal-holophrases+lateral-inhibition)
                     (:composer-strategy . :store-past-scenes)
                     (:hide-type-hierarchy . t)
                     (:confidence-threshold . 1.1)
                     (:cxn-decf-score . 0.3)
                     (:tutor-mode . :smart)
                     (:tutor-counts-failure-as . 2)))
                   (tutor-counts-1
                    ((:question-sample-method . :first)
                     (:questions-per-challenge . 5000)
                     (:scenes-per-question . 20)
                     (:alignment-strategy . :minimal-holophrases+lateral-inhibition)
                     (:composer-strategy . :store-past-scenes)
                     (:hide-type-hierarchy . t)
                     (:confidence-threshold . 1.1)
                     (:cxn-decf-score . 0.3)
                     (:tutor-mode . :smart)
                     (:tutor-counts-failure-as . 1)))
                   )
                 :number-of-interactions 10000
                 :number-of-series 3
                 :monitors (list ;; success
                                 "export-communicative-success"
                                 ;; overall lexicon size
                                 "export-lexicon-size"
                                 ;; overall cxn scores
                                 "export-avg-cxn-score"
                                 ;; lexical size per type
                                 "plot-lexicon-size-per-type" 
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
                                 "export-type-hierarchy"
                                 ;; print dots
                                 "print-a-dot-for-each-interaction"
                                 ))

