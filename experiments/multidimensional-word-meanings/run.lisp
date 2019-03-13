
(ql:quickload :mwm)
(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)
;(deactivate-monitor trace-interaction-in-web-interface)

(activate-monitor print-a-dot-for-each-interaction)

;; Creating the experiment might take a few seconds
;; since large amounts of data need to be loaded from file
(defparameter *experiment*
  (make-instance 'mwm-experiment))

(run-interaction *experiment*)

;; indicative features (= features with a high score) vs. discriminatory features
;; (= features that are sufficiently distant from other concepts)

(run-series *experiment* 100000)

(show-learner-lexicon (find 'learner (population *experiment*) :key #'id))

(lexicon-quality (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (baseline ())
                  )
                 :number-of-interactions 20000
                 :number-of-series 1)

(create-x-pos-convergence-graph :nr-of-interactions 100)

(create-graph-for-single-strategy
 :experiment-name "baseline"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-for-single-strategy
 :experiment-name "baseline"
 :measure-names '("lexicon-quality"
                  "meanings-per-form"
                  "utterance-length")
 :y-axis '(1 1 1)
 :y1-max nil
 :xlabel "Number of games")
  