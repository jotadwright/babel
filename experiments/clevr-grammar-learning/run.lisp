
(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)


;;;; MONITORS
;;;; --------
(progn
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  (activate-monitor trace-tasks-and-processes)
  (activate-monitor display-communicative-success))

(progn
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor display-metrics))

(deactivate-all-monitors)

(progn
  (deactivate-monitor trace-fcg)
  (deactivate-monitor trace-irl)
  (deactivate-monitor trace-interactions-in-wi))

(progn
  (deactivate-monitor display-communicative-success))


(let ((experiment-name 'basic-function-test))
  (run-experiments `(
                     (,experiment-name
                      ((:determine-interacting-agents-mode . :tutor-learner)
                       (:question-sample-mode . :random)
                       (:questions-per-challenge . 100)
                       (:confidence-threshold . 1.1)
                       (:cxn-decf-score . 0.3)))
                     )
                   :number-of-interactions 100
                   :number-of-series 2
                   :monitors (append (get-all-lisp-monitors)
                                     (get-all-csv-monitors)
                                     (list "print-a-dot-for-each-interaction"))))
