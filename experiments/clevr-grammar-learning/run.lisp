
(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)


(let ((experiment-name 'basic-function-test))
  (run-experiments `(
                     (,experiment-name
                      ((:determine-interacting-agents-mode . :tutor-learner)
                       (:question-sample-mode . :random)
                       (:questions-per-challenge . 100)
                       (:confidence-threshold . 0.9)
                       (:cxn-decf-score . 0.3)))
                     )
                   :number-of-interactions 100
                   :number-of-series 2
                   :monitors (append '("print-a-dot-for-each-interaction")
                                   ;(get-all-lisp-monitors)
                                   (get-all-export-monitors)
                                   )))
