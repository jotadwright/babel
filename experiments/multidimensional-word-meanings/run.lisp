
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
              (:determine-interacting-agents-mode . :tutor-speaks))))

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
                 :configuration *incremental-extracted*))

(run-interaction *experiment*)

(run-series *experiment* 100)

(display-lexicon (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(make-configuration
   :entries `((:experiment-type . :cogent)
              (:data-type . :simulated)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . ,500)
              (:data-sets . ,*cogent-simulated-data-sets*)))

(run-experiments `(
                   (test
                    ((:experiment-type . :baseline)
                     (:world-type . :extracted)
                     (:determine-interacting-agents-mode . :tutor-speaks)))
                   )
                 :number-of-interactions 2500
                 :number-of-series 3
                 :monitors (list "export-communicative-success"
                                 "export-lexicon-size"
                                 ;"export-features-per-form"
                                 ;"export-lexicon-evolution"
                                 ;"export-tutor-utterance-length-1"
                                 ;"export-tutor-utterance-length-2"
                                 ;"export-tutor-utterance-length-3"
                                 ;"export-tutor-utterance-length-4"
                                 ;"export-tutor-uses-xpos"
                                 ;"export-tutor-uses-ypos"
                                 ;"export-tutor-uses-color"
                                 ;"export-tutor-uses-size"
                                 ;"export-tutor-uses-material"
                                 ;"export-tutor-uses-shape"
                                 ))

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("communicative-success"
                  "lexicon-size")
 :average-windows '(100 1)
 :y-axis '(1 2)
 :y1-max 1
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





(create-graph-for-single-strategy
 :experiment-name "test-extracted-filter-one"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-comparing-strategies
 :experiment-names '("final/experiment-type-baseline-data-type-extracted"
                     "final/max-tutor-utterance-length-4-experiment-type-baseline-data-type-extracted")
 :measure-name "communicative-success"
 :y-min 0 :y-max 1 :xlabel "Number of games" :y1-label "Communicative Success"
 :title nil :captions '("1 word" "up to 4 words")
 :start 0 :end 2500 :window 250)


(create-grouped-bars-comparing-strategies
 :experiment-names '("final/max-tutor-utterance-length-4-experiment-type-baseline-data-type-simulated")
 :measure-names '("tutor-utterance-length-1"
                  "tutor-utterance-length-2"
                  "tutor-utterance-length-3"
                  "tutor-utterance-length-4")
 :cluster-labels '("tutor word use")
 :bar-labels '("1 word" "2 words" "3 words" "4 words")
 )

;; ------------------------------------------
;; + Running experiments for alist monitors +
;; ------------------------------------------

(create-tutor-word-use-graph
 :configurations (entries *baseline-simulated-configuration*)
 :nr-of-interactions 5000)

(create-learner-attribute-use-graph
 :configurations (entries *baseline-simulated-configuration*)
 :nr-of-interactions 5000)

(create-success-per-attribute-type-graph
 :configurations (entries *baseline-simulated-configuration*)
 :nr-of-interactions 5000)

(create-game-outcome-graph
 :configurations `((:experiment-type . :baseline)
                   (:data-type . :extracted)
                   (:scale-world . ,nil)
                   (:category-representation . :prototype)
                   (:determine-interacting-agents-mode . :learner-speaks-after-training-period)
                   (:training-period . 2000)
                   (:data-sets . ,*baseline-simulated-data-sets*)
                   (:data-path . ,*baseline-extracted-data-path*)
                   (:max-tutor-utterance-length . ,4)
                   (:extracted-colour-space . :lab)
                   (:alignment-filter . :all))
 :nr-of-interactions 5000)
