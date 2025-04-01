
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Script for running a demo of the concept-learning experiment ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load the package
(ql:quickload :cle)
(in-package :cle)

;; Setup the concept learning experiment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (format t "~%Starting a new experiment.~%")
  ;; reset the web interface
  (wi::reset)
  ;; reset monitors
  (monitors::notify reset-monitors)
  ;; deactivate all monitors (as a sanity check)
  (deactivate-all-monitors)
  ;; configure the game
  (defparameter *naming-game*
    (make-configuration
     :entries `(
                ;; monitoring
                (:log-every-x-interactions . 1000) ;; integer, frequence of when to log measures to standard output
                (:usage-table-window . 100) ;; integer, window size of the construction inventory usage table
                (:dataset-loader . :naming-game)
                (:dataset "naming-game")
                (:dataset-split . "train")
                (:dataset-view . :shared-views)
                ;; setup game
                (:interacting-agents-strategy . :boltzmann-partner-selection) ;; :standard, :boltzmann-partner-selection
                ;; population
                (:population-size . 100) ;; integer, size of the population
                (:world-size . 100)
                (:network-topology . :small-world) ;; :fully-connected, :regular, :small-world
                (:local-connectivity . 20) ;; for :regular and :small-world
                (:rewiring-probability . 0.3) ;; for :small-world
                (:boltzmann-tau . -15)
                (:boltzmann-lr . 0.05)
                (:min-context-size . 10) ;; integer, minimum number of context elements
                (:max-context-size . 10) ;; integer, maximum number of context elements
                ;; scene sampling
                (:scene-sampling . :random) ;; :random [AT THE MOMENT, ONLY OPTION AVAILABLE]
                (:topic-sampling . :random) ;; :random [AT THE MOMENT, ONLY OPTION AVAILABLE]
                ;; alignment
                (:initial-cxn-entrenchement . 0.5) ;; default entrenchment score
                (:align . t) ;; t or nil, activates alignment or not
                (:entrenchment-incf . 0.1) ;; hyperparameter for alignment (:align)
                (:entrenchment-decf . -0.1) ;; hyperparameter for alignment (:align)
                (:entrenchment-li . -0.1) ;; lateral inhibition, hyperparameter for alignment (:align)
                ;; measures
                (:coherence-perspective . :hearer) ;; :hearer or :speaker, determines how conventionalisation is measured
                ;; paths for exporting data to disk
                (:exp-top-dir . "a")  ;; directory name that groups a set of related of experiments together (e.g. logging/train/<exp-top-dir>)
                (:exp-name . "b")     ;; directory name that groups different runs of an experiment (i.e. logging/train/<exp-top-dir>/<exp-name>)
                (:log-dir-name . "c") ;; directory name for a single run (i.e. logging/train/<exp-top-dir>/<exp-name>/<log-dir-name>)
                )))
  ;; instantiate the concept learning experiment
  (setf *experiment* (make-instance 'cle-experiment :configuration *naming-game*)))


;; create visualisation of population
;(setf output-fname (population-network->graphviz (agents *experiment*) :layout "sfdp" :use-labels? t))
;(draw-graphviz-image output-fname :layout "sfdp" :make-image t :open-image t)


;; Option 1: run experiment for x interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  ;; reset the web interface
  (wi::reset)
  ;; deactivate `tracer` monitors
  (monitors::notify reset-monitors)
  (deactivate-all-monitors)
  ;; activate monitors

  (set-up-monitors (list "log-every-x-interactions-in-output-browser"
                         "record-communicative-success"
                         "record-conventionalisation"
                         "record-construction-inventory-usage"
                         "export-communicative-success"
                         "export-conventionalisation"
                         "export-construction-inventory-usage")
                   (list "train" "val")
                   *experiment*)

  (format t "~% --> Running a new serie of interactions.~%")
  (let ((nr-of-interactions 100000))
    (set-configuration *experiment* :nr-of-interactions nr-of-interactions)
    (run-interaction *experiment*)
    (while (< (interaction-number (current-interaction *experiment*)) nr-of-interactions)
           do (run-interaction *experiment*))
    (notify run-series-finished *experiment*)))




;; Option 2: run experiment and trace interactons in the web interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (run-series *experiment* 1))


;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; display the lexicon of an agent
(display-lexicon (first (agents *experiment*)) :weight-threshold 0.8 :sort t)

;; restore an experiment (.store) and initialise the world
(let ((fdir (babel-pathname :directory '("experiments"
                                          "concept-emergence2"
                                          "logging"
                                          "..."           ;; to update
                                          "stores"))))
  (setf *experiment* (load-experiment fdir "seed-?"))     ;; to update

  (test-stored-experiment *experiment*)
  ;; changes the dataset split
  ;(set-configuration *experiment* :dataset-split "test")
  
  ;; set alignment on (t) or off (nil)
  ;(set-configuration *experiment* :align nil)
  
  ;; initialise the world
  (initialise-world *experiment*))
