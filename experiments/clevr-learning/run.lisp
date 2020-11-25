
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;;;; MONITORS
;;;; --------
(progn
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))


;;;; SINGLE EXPERIMENT
;;;; -----------------

(defparameter *experiment*
  (make-instance 'clevr-learning-experiment
                 :entries '((:questions-per-challenge . 10))))

(run-interaction *experiment*)

(run-series *experiment* 25)


;;;; EXPERIMENT WITH CONFIGURATIONS
;;;; ------------------------------

(run-experiments '(
                   (test ((:questions-per-challenge . 10)))
                   )
                 :number-of-interactions 10
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 "export-lexicon-size"
                                 "export-meanings-per-form"
                                 "export-lexicon-change"
                                 "export-avg-cxn-score"
                                 "export-confidence-level"))

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("communicative-success"
                  "lexicon-size"
                  "meanings-per-form"
                  "lexicon-change"
                  "avg-cxn-score"
                  "confidence-level")
 :y-axis '(1 2 2 1 1 1)
 :y1-max 1 :y2-max nil)