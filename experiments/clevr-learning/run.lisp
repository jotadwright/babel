
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; run an experiment and immediately create the missing plots
(let ((experiment-name 'learner-speaks-best-strategy))
  (run-experiments `(
                     (,experiment-name
                      ((:speaker-sample-mode . :random)
                       (:learner-cxn-supplier . :scores)
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
                                   (get-all-export-monitors)))
  #|
  (create-graph-for-single-strategy
   :experiment-name (mkstr experiment-name)
   :measure-names '("communicative-success" "lexicon-size")
   :y-axis '(1 2) :y1-max 1 :open nil)
  (create-graph-for-single-strategy
   :experiment-name (mkstr experiment-name)
   :measure-names '("lexical-meanings-per-form" "lexical-forms-per-meaning")
   :y-axis '(1) :y1-max nil :open nil)
  |#
)
                 

