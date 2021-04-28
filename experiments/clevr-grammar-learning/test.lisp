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
                            (:observation-sample-size . 500) ;; number of observations to sample
                            (:observation-sample-mode . :random) ;; random first or all
                            (:determine-interacting-agents-mode . :tutor-learner)
                            (:learner-th-connected-mode . :path-exists))))
                             


;;; test single interaction
(run-interaction *experiment*)


;;; test series of interactions
(progn
  (wi::reset)
  (run-series *experiment* 200))

