
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
  
  ;; deactivate all monitors (as a sanity check)
  (deactivate-all-monitors)
  ;; configure the game
  (defparameter *concept-learning-game*
    (make-configuration
     :entries `(
                ;; monitoring
                (:log-every-x-interactions . 10) ;; integer, frequence of when to log measures to standard output
                (:usage-table-window . 100) ;; integer, window size of the construction inventory usage table
                (:save-distribution-history . nil) ;; t or nil, whether to save the history of updates to the distribution (very memory-intensive!)
                ;; setup environment
                (:dataset-loader . :precomputed) ;; :precomputed or :runtime, load data in by scene (:precomputed) or by objects (:runtime)
                (:dataset-view . :shared-views) ;; :shared-views or :exclusive-views, all views are shared or is each agent assigned a view?
                (:dataset "clevr") ;; list of strings, each string represents a view over a dataset
                (:feature-set "clevr") ;; list of strings, each string represents a feature set (stored in Corpora/concept-emergence2/-feature-sets), every feature-set is associated to a coressponding element in :dataset
                (:dataset-split . "train") ;; string, "train" or "test", which split of the data to use?
                ;; setup game
                (:interacting-agents-strategy . :standard) ;; :standard [AT THE MOMENT, ONLY OPTION AVAILABLE]
                (:population-size . 10) ;; integer, size of the population
                (:min-context-size . 10) ;; integer, minimum number of context elements
                (:max-context-size . 10) ;; integer, maximum number of context elements
                ;; disable channels
                (:disable-channels . :none) ;; :none [AT THE MOMENT, ONLY OPTION AVAILABLE]
                (:amount-disabled-channels . 0) ;; integer, amount of channels to disable
                ;; noised channels
                (:sensor-noise . :none) ;; :none or :shift
                (:sensor-std . 0.0) ;; float, corresponds to calibration noise
                (:observation-noise . :none) ;; :none or :shift
                (:observation-std . 0.0) ;; float, noise during perception
                ;; scene sampling
                (:scene-sampling . :random) ;; :random [AT THE MOMENT, ONLY OPTION AVAILABLE]
                (:topic-sampling . :random) ;; :random [AT THE MOMENT, ONLY OPTION AVAILABLE]
                ;; alignment
                (:initial-cxn-entrenchement . 0.5) ;; default entrenchment score
                (:align . t) ;; t or nil, activates alignment or not
                (:entrenchment-incf . 0.1) ;; hyperparameter for alignment (:align)
                (:entrenchment-decf . -0.1) ;; hyperparameter for alignment (:align)
                (:entrenchment-li . -0.02) ;; lateral inhibition, hyperparameter for alignment (:align)
                ;; concept representation parameters
                (:M2 . 0.0001) ;; float, default initialisation for gaussian distributions
                ;; prototype weight inits
                (:weight-update-strategy . :j-interpolation) ;; :standard or :j-interpolation
                (:initial-weight . 0) ;; default weight
                (:weight-incf . 1)    ;; :standard uses floats, j-interpolation uses int
                (:weight-decf . -5)   ;; :standard uses floats, j-interpolation uses int
                ;; experimental alternatives
                (:prototype-distance . :paper) ;; :paper or :paper-wo-ledger
                ;; staging
                (:switch-condition . :none) ; :none [AT THE MOMENT, ONLY OPTION AVAILABLE]
                (:switch-conditions-after-n-interactions . 0) ;; [AT THE MOMENT, ONLY OPTION AVAILABLE]
                (:stage-parameters nil) ;; [AT THE MOMENT, ONLY OPTION AVAILABLE]
                ;; measures
                (:coherence-perspective . :hearer) ;; :hearer or :speaker, determines how conventionalisation is measured
                ;; paths for exporting data to disk
                (:exp-top-dir . "a")  ;; directory name that groups a set of related of experiments together (e.g. logging/train/<exp-top-dir>)
                (:exp-name . "b")     ;; directory name that groups different runs of an experiment (i.e. logging/train/<exp-top-dir>/<exp-name>)
                (:log-dir-name . "c") ;; directory name for a single run (i.e. logging/train/<exp-top-dir>/<exp-name>/<log-dir-name>)
                )))
  ;; instantiate the concept learning experiment
  (setf *experiment* (make-instance 'cle-experiment :configuration *concept-learning-game*))
  ;; flush the monitor data
  (notify reset-monitors))

;; Option 1: run experiment for x interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  ;; reset the web interface
  (wi::reset)
  ;; deactivate `tracer` monitors
  (deactivate-all-monitors)
  ;; activate monitors
  (activate-monitor record-communicative-success)
  (activate-monitor record-conventionalisation)
  (activate-monitor log-every-x-interactions-in-output-browser)
  (format t "~% --> Running a new serie of interactions.~%")
  (time (run-series *experiment* 100000)))

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
  ;; changes the dataset split
  ;(set-configuration *experiment* :dataset-split "test")
  
  ;; set alignment on (t) or off (nil)
  ;(set-configuration *experiment* :align nil)
  
  ;; initialise the world
  (initialise-world *experiment*))