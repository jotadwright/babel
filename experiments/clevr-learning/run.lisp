
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; run an experiment and immediately create the missing plots
(let ((experiment-name 'learner-speaks-confidence-02))
  (run-experiments `(
                     (,experiment-name
                      ((:determine-interacting-agents-mode . :default)
                       (:learner-speaks-confidence-threshold . 0.2)
                       (:question-sample-mode . :first)
                       (:questions-per-challenge . 100)
                       (:scenes-per-question . 50)
                       (:confidence-threshold . 1.1)
                       (:speaker-sample-mode . :random)
                       (:cxn-decf-score . 0.3)))
                     )
                   :number-of-interactions 100
                   :number-of-series 2
                   :monitors (append (get-all-lisp-monitors)
                                     (get-all-csv-monitors)
                                     (list "print-a-dot-for-each-interaction")))
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
                 

