(ql:quickload :grammar-learning)
(in-package :grammar-learning)



(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (make-instance 'grammar-learning-experiment
                   :entries '((:observation-sample-mode . :sort-length-ascending)
                              (:meaning-representation . :amr)))))