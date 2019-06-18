
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

(run-series *experiment* 3000)

(display-lexicon (find 'learner (population *experiment*) :key #'id))
(display-lexicon (find 'tutor (population *experiment*) :key #'id))
(lexicon->function-plots (find 'learner (population *experiment*) :key #'id))

(make-table *experiment*)

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
                 :number-of-series 5
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
 :experiment-names '("continuous-tutor-min-max"
                     "continuous-tutor-prototype"
                     "continuous-tutor-prototype-min-max"
                     "continuous-tutor-exponential")
 :measure-name "communicative-success"
 :y-min 0 :y-max 1 :xlabel "Number of games" :y1-label "Success"
 :captions '("min-max" "prototype" "prototype-min-max" "exponential")
 :title "baseline" :end 5000)
