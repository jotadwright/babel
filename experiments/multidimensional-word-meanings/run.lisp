
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
   :entries '((:perceptual-deviation . t)
                     (:game-mode . :tutor-learner)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:tutor-lexicon . :continuous)
                     (:category-representation . :prototype)
                     (:lexical-variation . nil)
                     (:feature-selection . :all)
                     (:noise-amount . nil)
                     (:noise-prob . nil)
                     (:scale-world . t))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)

(run-series *experiment* 100)

(run-series *experiment* 3000)

(display-lexicon (find 'learner (population *experiment*) :key #'id))
(display-lexicon (find 'tutor (population *experiment*) :key #'id))
(lexicon->function-plots (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (test
                    ((:perceptual-deviation . t)
                     (:game-mode . :tutor-learner)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:tutor-lexicon . :continuous)
                     (:category-representation . :prototype)
                     (:lexical-variation . nil)
                     (:feature-selection . :all)
                     (:noise-amount . nil)
                     (:noise-prob . nil)
                     (:scale-world . t)))
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

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("lexicon-size")
 :y-axis '(1)
 :y1-max nil
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-comparing-strategies
 :experiment-names '("continuous-tutor-exponential"
                     "exponential-perceptual-deviation")
 :measure-name "communicative-success"
 :y-max 1 :xlabel "Number of games" :y1-label "Success"
 :captions '("same perspective" "different perspective")
 :end 20000)

(create-graph-comparing-strategies
 :experiment-names '("min-max-perceptual-deviation"
                     "prototype-perceptual-deviation"
                     "pmm-perceptual-deviation"
                     "exponential-perceptual-deviation")
 :measure-name "communicative-success"
 :y-min 0.8 :y-max 1 :xlabel "Number of games" :y1-label "Success"
 :captions '("min-max" "prototype" "pmm" "exponential")
 :end 20000)
 


  
