
(ql:quickload :mwm)
(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)
;(deactivate-monitor trace-interaction-in-web-interface)

(activate-monitor print-a-dot-for-each-interaction)

(activate-monitor display-communicative-success)
;(deactivate-monitor display-communicative-success)

;; --------------------
;; + Run interactions +
;; --------------------

;;;; CONFIGURATIONS
(defparameter *baseline-simulated*
  (make-configuration
   :entries '((:experiment-type . :baseline)
              (:world-type . :simulated)
              (:determine-interacting-agents-mode . :default))))

(defparameter *baseline-extracted*
  (make-configuration
   :entries '((:experiment-type . :baseline)
              (:world-type . :extracted)
              (:determine-interacting-agents-mode . :tutor-speaks))))

(defparameter *cogent-simulated*
  (make-configuration
   :entries '((:experiment-type . :cogent)
              (:world-type . :simulated)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 100))))

(defparameter *cogent-extracted*
  (make-configuration
   :entries '((:experiment-type . :cogent)
              (:world-type . :extracted)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 100))))

(defparameter *incremental-simulated*
  (make-configuration
   :entries '((:experiment-type . :incremental)
              (:world-type . :simulated)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 100))))

(defparameter *incremental-extracted*
  (make-configuration
   :entries '((:experiment-type . :incremental)
              (:world-type . :extracted)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 100))))

;;;; EXPERIMENT
(defparameter *experiment*
  (make-instance 'mwm-experiment
                 :configuration *baseline-extracted*))

(run-interaction *experiment*)

(run-series *experiment* 100)

(display-lexicon (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments `(
                   (test
                    ((:experiment-type . :baseline)
                     (:world-type . :simulated)
                     (:determine-interacting-agents-mode . :default)))
                   )
                 :number-of-interactions 5000
                 :number-of-series 3
                 :monitors (list "export-communicative-success"
                                 "export-lexicon-size"
                                 "export-communicative-success-given-conceptualisation"
                                 ;"export-learner-concepts"
                                 ))

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("communicative-success"
                  "communicative-success-given-conceptualisation"
                  "lexicon-size"
                  )
 :average-windows '(200 200 1)
 :y-axis '(1 1 2)
 :y1-min 0 :y1-max 1
 :y2-max nil
 :xlabel "Number of games"
 :y1-label "Communicative Success"
 :y2-label "Concept Repertoire Size"
 :open t)

(create-tutor-word-use-graph
 :configurations
 '((:experiment-type . :baseline)
   (:world-type . :extracted)
   (:determine-interacting-agents-mode . :tutor-speaks))
 :nr-of-interactions 2500)

(create-learner-failed-conceptualisation-graph
 :configurations
 '((:experiment-type . :baseline)
   (:world-type . :extracted)
   (:determine-interacting-agents-mode . :default))
 :nr-of-interactions 5000)
