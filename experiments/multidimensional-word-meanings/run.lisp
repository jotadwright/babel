
(ql:quickload :mwm)
(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)
;(deactivate-monitor trace-interaction-in-web-interface)

(activate-monitor print-a-dot-for-each-interaction)

;; Creating the experiment might take a few seconds
;; since large amounts of data need to be loaded from file
(defparameter *configuration*
  (make-configuration
   :entries '((:shift-prototype . :always)
              (:strategy . :min-max)
              (:noise . nil))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)

;; TO DO; features reaching 0 certainty should be removed
;; However, there should be a way to add features again, when needed.
;; This will be necessary for the CLEVR COGENT experiment.
;; The features should be added again when the game fails.
;; How to decide which features to add again?

(run-series *experiment* 10)

(show-learner-lexicon (find 'learner (population *experiment*) :key #'id))

(lexicon-quality (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (baseline ((:shift-prototype . :always)
                              (:strategy . :min-max)
                              (:noise . nil)))
                  )
                 :number-of-interactions 10000
                 :number-of-series 5
                 :monitors (list "export-communicative-success"
                                 "export-lexicon-size"
                                 "export-features-per-form"
                                 "export-utterance-length"))

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
  
