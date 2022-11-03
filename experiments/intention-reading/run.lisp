
(ql:quickload :clevr-learning)
(in-package :intention-reading)

;; run an experiment and immediately create the missing plots
(let ((experiment-name 'learner-speaks-best-strategy))
  (run-experiments `(
                     (,experiment-name
                      ((:speaker-sample-mode . :smart)
                       (:learner-cxn-supplier . :ordered-by-label-and-score)
                       (:alignment-strategy . :lateral-inhibition)
                       (:determine-interacting-agents-mode . :default)
                       (:question-sample-mode . :all)
                       (:scenes-per-question . 50)
                       (:confidence-threshold . 1.1)
                       (:primitives . :symbolic)
                       (:cxn-decf-score . 0.3)
                       (:learner-speaks-confidence-threshold . 0.2)))
                     )
                   :number-of-interactions 50000
                   :number-of-series 10
                   :monitors (cons "print-a-dot-for-each-interaction"
                                   (get-all-export-monitors))))
                 

