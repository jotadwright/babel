(ql:quickload :pattern-finding)
(in-package :pf)

;; activate verbose monitors
(progn
  (monitors::deactivate-all-monitors)
  (monitors::activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  (activate-monitor trace-interactions-in-wi-verbose))

;; make the experiment
(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:comprehend-all-n . 2)
                              (:shuffle-data-p . t)
                              (:number-of-epochs . 20)
                              (:repair-recursively . nil)
                              (:allow-cxns-with-no-strings . nil)
                              (:max-nr-of-nodes . 5000)
                              (:alignment-strategy . :lateral-inhibition)
                              (:anti-unification-mode . :heuristic)
                              (:partial-analysis-mode . :heuristic)
                              (:push-meets-to-deltas . t)
                              (:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning" "cooking" "data")))
                              (:corpus-file . ,(make-pathname :name "benchmark-ingredients-cleaned" :type "jsonl"))))))

;; run a few interactions
(run-series *experiment* 5)

;; activate summary monitors
(progn
  (monitors::deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor display-metrics))

;; run more interactions
(run-series *experiment* (* 19 263))

;; activate verbose monitors again
(progn
  (monitors::deactivate-all-monitors)
  (monitors::activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  (activate-monitor trace-interactions-in-wi-verbose))

;; run interactions and inspect learned cxns
(run-series *experiment* 10)