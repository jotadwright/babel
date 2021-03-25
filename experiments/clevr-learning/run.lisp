
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; run an experiment and immediately create the missing plots
(let ((experiment-name 'new-cxn-supplier))
  
  (run-experiments `(
                     (,experiment-name
                      ((:question-sample-mode . :first)
                       (:questions-per-challenge . 100)
                       (:scenes-per-question . 20)
                       (:confidence-threshold . 1.1) ;; force to stay in level 1
                       (:cxn-decf-score . 0.2)
                       (:tutor-mode . :smart)
                       (:tutor-counts-failure-as . 1)))
                     )
                   :number-of-interactions 100
                   :number-of-series 2
                   :monitors (append '("print-a-dot-for-each-interaction")
                                     (get-all-lisp-monitors)
                                     (get-all-csv-monitors)
                                     (get-all-export-monitors)))
  
  (create-graph-for-single-strategy
   :experiment-name (mkstr experiment-name)
   :measure-names '("communicative-success" "lexicon-size")
   :y-axis '(1 2) :y1-max 1 :open nil)
  
  (create-graph-for-single-strategy
   :experiment-name (mkstr experiment-name)
   :measure-names '("lexical-meanings-per-form" "lexical-forms-per-meaning")
   :y-axis '(1) :y1-max nil :open nil)

)
                 

