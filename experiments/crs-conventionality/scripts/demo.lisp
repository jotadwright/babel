;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;; Script for running a quick demo of the crs-conventionality experiments ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the system and set *package*
(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting the configurations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Baseline: canonical naming game 
(defparameter *configuration* (make-configuration
                               :entries '(;; Logging
                                          (:log-every-x-interactions . 100)
                                          ;; Initialising the experiment
                                          (:nr-of-entities-in-world . 10)
                                          (:nr-of-agents-in-population . 10)
                                          (:nr-of-entities-in-scene . 5)
                                          (:alignment-strategy . :lateral-inhibition)
                                          (:learning-strategy . :default)
                                          (:learning-rate . 0.5)
                                          (:neighbor-q-value-lr . 0.01)
                                          ;; Initialising an interaction
                                          (:determine-interacting-agents-mode . :random-from-social-network)
                                          (:determine-scene-entities-mode . :random-subset-of-world)
                                          (:determine-topic-mode . :random-entity-from-scene))))

; Evaluate one or more of the following expressions to adapt the *configuration* of the naming game

;; Social network
(set-configurations *configuration* '((:network-topology . :regular)
                                      (:local-connectivity . 1)))


;; Boltzmann partner selection
(set-configurations *configuration* '((:determine-interacting-agents-mode . :boltzmann-partner-selection)
                                      (:boltzmann-tau . -20)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instantiating the experiment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (format t "~%Starting a new experiment.~%")
  ;; reset the web interface
  (wi::reset)
  ;; deactivate all monitors (as a sanity check)
  (monitors::notify reset-monitors)
  ;; instantiate a naming game experiment
  (reset-id-counters)
  (defparameter *naming-game* (make-instance 'naming-game-experiment
                                             :configuration *configuration*))
  ;; visualise the population network
  (population-network->graphviz (agents (population *naming-game*)) :make-image t :open-image nil :use-labels? t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running the experiment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Option 1: run experiment with real-time plotting (using gnuplot)
(progn
  ;; reset monitors
  (deactivate-all-monitors)
  
  ;; activate monitors
  (activate-monitor log-every-x-interactions-in-output-browser)
  (activate-monitor record-communicative-success)
  (activate-monitor record-conventionalisation)
  (activate-monitor record-construction-inventory-size)

  ;; activate the gnuplot live display
  (activate-monitor display-metrics)

  ;; run the experiment
  (loop for i from 1 to 5000
        do (run-interaction *naming-game*)))


;; Option 2: run experiment with real-time tracing in the web interface
(progn
  ;; reset monitors
  (deactivate-all-monitors)
 
  ;; activate tracers
  (activate-monitor trace-interaction)
  (activate-monitor trace-fcg-crs)
  (activate-monitor trace-irl-crs)

  ;; run the experiment
  (loop for i from 1 to 10
        do (run-interaction *naming-game*)))


;; Option 3: run experiment locally and export results to disk
(progn
  ;; reset monitors
  (deactivate-all-monitors)
  
  ;; activate monitors
  (activate-monitor log-every-x-interactions-in-output-browser)
  (activate-monitor export-communicative-success)
  (activate-monitor export-conventionalisation)
  (activate-monitor export-construction-inventory-size)
  
  ;; run the experiment
  (run-batch 'naming-game-experiment ;; experiment-class
             5000 ;; nr-of-interactions
             1 ;; nr-of-series
             :configuration *configuration*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introducing a new agent into the population ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Introduce new agent that will always be selected as the listener 
(progn
  (introduce-new-agents *naming-game* :number-of-agents 1)
  (set-configuration *naming-game* :determine-interacting-agents-mode :random-listener-from-younger-generation)
  (population-network->graphviz (agents (population *naming-game*)) :make-image t :open-image t :use-labels? t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Population Turnover/Replacement ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (replace-agents *naming-game* 0.5)
  (population-network->graphviz (agents (population *naming-game*)) :make-image t :open-image t :use-labels? t))