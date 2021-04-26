(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

(defparameter *experiment*
   (make-instance 'clevr-grammar-learning-experiment
                  :entries '((:learner-cxn-supplier . :ordered-by-label-and-score)
                             (:question-sample-mode . :random)
                             (:determine-interacting-agents-mode . :tutor-learner)
                             )))

(run-interaction *experiment*)

(interacting-agents *experiment*)

(question-data *experiment*)