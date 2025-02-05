;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;; Script for running a quick demo of the crs-conventionality experiments ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the system and set *package*
(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)


;; Canonical naming game setting ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (format t "~%Starting a new experiment.~%")
  ;; reset the web interface
  (wi::reset)
  ;; deactivate all monitors (as a sanity check)
  (notify reset-monitors)
  ;; configure a canonical naming game
  (defparameter *configuration-canonical* (make-configuration
                                           :entries '(;; Logging
                                                      (:log-every-x-interactions . 100)
                                                      ;; Initialising the experiment
                                                      (:nr-of-entities-in-world . 10)
                                                      (:nr-of-agents-in-population . 10)
                                                      (:nr-of-entities-in-scene . 5)
                                                      (:alignment-strategy . :lateral-inhibition)
                                                      (:learning-strategy . :default)
                                                      (:learning-rate . 0.1)
                                                      ;; Initialising an interaction
                                                      (:determine-interacting-agents-mode . :random-from-population)
                                                      (:determine-scene-entities-mode . :random-subset-of-world)
                                                      (:determine-topic-mode . :random-entity-from-scene))))
  ;; instantiate a naming game experiment
  (defparameter *naming-game-canonical* (make-instance 'naming-game-experiment
                                                       :configuration *configuration-canonical*)))


;; Option 1: run experiment with real-time plotting (using gnuplot)

(progn
  ;; reset monitors
  (deactivate-all-monitors)
  
  ;; activate monitors
  (activate-monitor log-every-x-interactions-in-output-browser)
  (activate-monitor record-communicative-success)
  (activate-monitor record-conventionalisation)
  (activate-monitor record-construction-inventory-size)

  ;; activate tracers
  ;(activate-monitor trace-interaction)

  ;; activate the gnuplot live display
  (flush-gnuplot-display 'display-metrics)
  (activate-monitor display-metrics)

  ;; run the experiment
  (loop for i from 1 to 1000
        do (run-interaction *naming-game-canonical*)))

;; Option 2: run experiment locally and export results to disk

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
             :configuration *configuration-canonical*))