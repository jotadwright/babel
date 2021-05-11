
(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)


(let ((experiment-name 'basic-function-test))
  (run-experiments `(
                     (,experiment-name
                      ((:determine-interacting-agents-mode . :tutor-learner)
                       (:observation-sample-mode . :random)
                       (:observation-sample-size . 500)
                       (:cxn-decf-score . 0.3)))
                     )
                   :number-of-interactions 100
                   :number-of-series 2
                   :monitors (append '("print-a-dot-for-each-interaction")
                                   (get-all-lisp-monitors)
                                   (get-all-export-monitors)
                                   )))
