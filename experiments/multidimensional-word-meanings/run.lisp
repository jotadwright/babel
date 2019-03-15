
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

;; TO DO; features reaching 0 certainty should be removed
;; However, there should be a way to add features again, when needed.
;; This will be necessary for the CLEVR COGENT experiment.

;; The features should be added again when the game fails.
;; How to decide which features to add again?

;; For now, all the ones that are no longer present are put
;; back but with a low initial certainty score. This gives very bad performance.

;; Maybe this should be based on some kind of re-entrance. If I had had this feature
;; with the value of the topic, would I have interpreted the topic correctly?

;; Or otherwise look at the scene and try to determine if the current (unused)
;; feature is important for the topic w.r.t. the other objects.
;; If it is, than add the feature again.

(run-series *experiment* 10)

(show-learner-lexicon (find 'learner (population *experiment*) :key #'id))

(lexicon-quality (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (baseline ())
                  )
                 :number-of-interactions 10000
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
 :measure-names '("features-per-form"
                  "utterance-length")
 :y-axis '(1 1 1)
 :y1-max nil
 :xlabel "Number of games")
  
