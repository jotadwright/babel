(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

(defparameter *experiment*
   (make-instance 'clevr-grammar-learning-experiment
                  :entries '((:learner-cxn-supplier . :ordered-by-label-and-score)
                             (:questions-per-challenge . 100) ;; number of observations
                             (:question-sample-mode . :random) ;; random first or all
                             (:determine-interacting-agents-mode . :tutor-learner)
                             )))

(run-interaction *experiment*)

(interacting-agents *experiment*)

(length (question-data *experiment*))