(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)


(progn
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  (activate-monitor trace-tasks-and-processes))

(progn
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor display-metrics))


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
(cdr (first (question-data *experiment*)))
(defparameter *curr* (current-interaction *experiment*))
(get-interaction-data (current-interaction *experiment*))