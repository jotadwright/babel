
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
  (defparameter *concept-learning-game*
    (make-configuration
     :entries `(
                ;; monitoring
                (:log-every-x-interactions . 100) ;; integer, frequence of when to log measures to standard output
                (:usage-table-window . 100) ;; integer, window size of the construction inventory usage table
                (:save-distribution-history . nil) ;; t or nil, whether to save the history of updates to the distribution (very memory-intensive!)
                ;; setup environment
                (:dataset-loader . :precomputed) ;; keyword, :precomputed or :runtime, load data in by scene (:precomputed) or by objects (:runtime)
                (:dataset-view . :shared-views) ;; keyword, :shared-views or :exclusive-views, all views are shared or is each agent assigned a view?
                (:dataset "clevr") ;; list of strings, each string represents a view over a dataset
                (:feature-set "clevr") ;; list of strings, each string represents a feature set (stored in Corpora/concept-emergence2/-feature-sets), every feature-set is associated to a coressponding element in :dataset
                (:dataset-split . "train") ;; string, "train" or "test", which split of the data to use?
                ;; population
                (:population-size . 2) ;; integer, size of the population
                (:network-topology . :fully-connected) ;; keyword, :fully-connected or :regular or :small-world
                ;(:local-connectivity . 2) ;; integer, for :regular and :small-world
                ;(:rewiring-probability . 0.3) ;; float, for :small-world
                (:learning-environment . :tutor-learner) ;; keyword, :emergence or tutor-learner
                (:interacting-agents-strategy . :tutor-learner) ;; keyword, :standard or :boltzmann-partner-selection or :tutor-learner
                (:boltzmann-tau . 0) ;; integer, for :boltzmann-partner-selection
                (:boltzmann-lr . 0.05) ;; float, for :boltzmann-partner-selection
                (:min-context-size . 10) ;; integer, minimum number of context elements
                (:max-context-size . 10) ;; integer, maximum number of context elements
                ;; disable channels
                (:disable-channels . :none) ;; keyword, :none or :random or :fixed
                (:amount-disabled-channels . 0) ;; integer, amount of channels to disable
                ;; noised channels
                (:sensor-noise . :none) ;; keyword, :none or :shift
                (:sensor-std . 0.0) ;; float, corresponds to calibration noise
                (:observation-noise . :none) ;; keyword, :none or :shift
                (:observation-std . 0.0) ;; float, noise during perception
                ;; scene sampling
                (:scene-sampling . :random) ;; :random
                (:topic-sampling . :discriminative) ;; :random, :discriminative
                ;; alignment
                (:initial-cxn-entrenchement . 0.5) ;; default entrenchment score
                (:align . t) ;; t or nil, activates alignment or not
                (:entrenchment-incf . 0.0) ;; hyperparameter for alignment (:align)
                (:entrenchment-decf . 0.0) ;; hyperparameter for alignment (:align)
                (:entrenchment-li . -0.0) ;; lateral inhibition, hyperparameter for alignment (:align)
                ;; concept representation parameters
                (:M2 . 0.001) ;; float, default initialisation for gaussian distributions
                ;; prototype weight inits
                (:weight-update-strategy . :j-interpolation) ;; :standard or :j-interpolation
                (:initial-weight . 0) ;; default weight
                (:weight-incf . 1)    ;; :standard uses floats, j-interpolation uses int
                (:weight-decf . -1)   ;; :standard uses floats, j-interpolation uses int
                ;; experimental alternatives
                (:prototype-distance . :paper) ;; :paper or :paper-wo-ledger
                ;; staging
                (:switch-condition . :none) ; :none, :after-n-interactions
                ;(:switch-conditions-after-n-interactions . 50000) ;; for :after-n-interactions
                ;(:stage-parameters ((:switch-disable-channels-half . 10))) ;; for :after-n-interactions
                ;; measures
                (:coherence-perspective . :hearer) ;; :hearer or :speaker, determines how conventionalisation is measured
                ;; paths for exporting data to disk
                (:exp-top-dir . "a")  ;; directory name that groups a set of related of experiments together (e.g. logging/train/<exp-top-dir>)
                (:exp-name . "b")     ;; directory name that groups different runs of an experiment (i.e. logging/train/<exp-top-dir>/<exp-name>)
                (:log-dir-name . "c") ;; directory name for a single run (i.e. logging/train/<exp-top-dir>/<exp-name>/<log-dir-name>)
                )))
  ;; instantiate the concept learning experiment
  (setf *experiment* (make-instance 'cle-experiment :configuration *concept-learning-game*)))


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

  (setf *success-per-concept* (make-hash-table :test #'equalp))

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

(defun display-success ()
  (loop for key being the hash-keys of *success-per-concept*
        using (hash-value counts)
        for num = (car counts)
        for denom = (cdr counts)
        do (format t "~%~,1f (~a)"
                   (* 100 (/ num denom))
                   key)))

(display-success)




;; Option 2: run experiment and trace interactons in the web interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (wi::reset)
  (deactivate-all-monitors)
  (activate-monitor trace-interaction-in-web-interface)
  (set-configuration *experiment* :nr-of-interactions 100000)
  (run-series *experiment* 1))


;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; display the lexicon of an agent
(display-lexicon (second (agents *experiment*)) :weight-threshold 0.1 :sort t)

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
