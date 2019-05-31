
(ql:quickload :mwm)
(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)
;(deactivate-monitor trace-interaction-in-web-interface)

(activate-monitor print-a-dot-for-each-interaction)

(activate-monitor display-communicative-success)
;(deactivate-monitor display-communicative-success)

;; Creating the experiment might take a few seconds
;; since large amounts of data need to be loaded from file
(defparameter *configuration*
  (make-configuration
   :entries '((:game-mode . :tutor-learner)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:tutor-lexicon . :continuous)
              (:category-representation . :prototype)
              (:max-tutor-utterance-length . 1)
              (:noise-amount . 0.2)
              (:noise-prob . 0.5)
              (:remove-on-lower-bound . nil))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)

(run-series *experiment* 100)

(run-series *experiment* 5000)

(display-lexicon (find 'learner (population *experiment*) :key #'id))
(display-lexicon (find 'tutor (population *experiment*) :key #'id))
(lexicon->function-plots (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (test
                    ((:remove-on-lower-bound . nil)
                     (:max-tutor-utterance-length . 1)
                     (:game-mode . :tutor-learner)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:tutor-lexicon . :continuous)
                     (:category-representation . :prototype)
                     (:noise-amount . 0.5)
                     (:noise-prob . 0.5)
                     (:tutor-re-entrance . nil)))
                   )
                 :number-of-interactions 5000
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 ;"export-lexicon-size"
                                 ;"export-features-per-form"
                                 ;"export-utterance-length"
                                 ))

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-comparing-strategies
 :experiment-names '("continuous-tutor-exponential"
                     "tutor-re-entrance-t-noise-amount-0.5-category-representation-exponential"
                     "remove-on-lower-bound-nil-noise-amount-0.5-category-representation-exponential")
 :measure-name "communicative-success"
 :y-min 0 :y-max 1 :xlabel "Number of games" :y1-label "Success"
 :captions '("baseline" "noise" "keep-categories")
 :title "exponential (p=0.5, n=0.5)" :end 20000)
