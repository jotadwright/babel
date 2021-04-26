
(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

(let ((experiment-name 'basic-function-test))
  (run-experiments `(
                     (,experiment-name
                      ((:determine-interacting-agents-mode . :tutor-learner)
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
                                     (list "print-a-dot-for-each-interaction"))))
