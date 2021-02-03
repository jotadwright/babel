
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; use this file to run experiments (possibly on the cluster)

(run-experiments '(
                   (test
                    ((:questions-per-challenge . 500)
                     (:alignment-strategy . :minimal-holophrases+lateral-inhibition)
                     (:composer-strategy . :store-past-scenes)
                     (:hide-type-hierarchy . t)
                     (:question-sample-method . :first)))
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
                                 ;; other metrics
                                 "export-lexicon-change"
                                 "export-confidence-level"
                                 ;; export data
                                 "export-type-hierarchy-every-nth-interaction"
                                 "export-learner-grammar"
                                 ;; print dots
                                 "print-a-dot-for-each-interaction"
                                 ))
    

#|
;; keep a list of "top-level arguments". After encountering
;; --configurations, parse everything as a configuration until
;; a top-level argument is encountered.

;; keep a conversion function for each argument, e.g. #'identity
;; or #'make-symbol or #'parse-integer, etc.
 
 sbcl --load run.lisp --quit
      --number-of-interactions 1000
      --number-of-series 1
      --experiment-name test
      --configurations
      --questions-per-challenge 500
      --alignment-strategy blabla
      --monitors
|#