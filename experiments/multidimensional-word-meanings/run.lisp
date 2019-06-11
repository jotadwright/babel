
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
   :entries '((:tutor-lexicon . :symbolic)
              (:data-source . :continuous-clevr)
              (:scale-world . nil)
              (:category-representation . :prototype)
              (:noise-amount . nil)
              (:noise-prob . nil))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)

(run-series *experiment* 10)

(run-series *experiment* 3000)

(display-lexicon (find 'learner (population *experiment*) :key #'id))
(display-lexicon (find 'tutor (population *experiment*) :key #'id))
(lexicon->function-plots (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (test
                    ((:tutor-lexicon . :symbolic)
                     (:data-source . :continuous-clevr)
                     (:scale-world . nil)
                     (:category-representation . :prototype)
                     (:noise-amount . nil)
                     (:noise-prob . nil)))
                   )
                 :number-of-interactions 10000
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
                     "noise-amount-0.05-category-representation-exponential"
                     "noise-amount-0.1-category-representation-exponential"
                     "noise-amount-0.2-category-representation-exponential"
                     "noise-amount-0.3-category-representation-exponential"
                     "noise-amount-0.4-category-representation-exponential"
                     "noise-amount-0.5-category-representation-exponential")
 :measure-name "communicative-success"
 :y-min 0 :y-max 1 :xlabel "Number of games" :y1-label "Success"
 :captions '("baseline" "n=0.05" "n=0.1" "n=0.2" "n=0.3" "n=0.4" "n=0.5")
 :title "exponential (p=0.5)" :end 20000)
