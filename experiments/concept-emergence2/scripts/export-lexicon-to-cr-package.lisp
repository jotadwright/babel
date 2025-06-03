
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                          ;;
;; PHASE 1: LEARNING GROUNDED CONSTRUCTIONS ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                (:usage-table-window . 5000) ;; integer, window size of the construction inventory usage table
                (:save-distribution-history . nil) ;; t or nil, whether to save the history of updates to the distribution (very memory-intensive!)
                ;; setup environment
                (:dataset-loader . :precomputed) ;; keyword, :precomputed or :runtime, load data in by scene (:precomputed) or by objects (:runtime)
                (:dataset-view . :shared-views) ;; keyword, :shared-views or :exclusive-views, all views are shared or is each agent assigned a view?
                (:dataset "clevr-simulated") ;; list of strings, each string represents a view over a dataset
                (:feature-set "clevr-simulated") ;; list of strings, each string represents a feature set (stored in Corpora/concept-emergence2/-feature-sets), every feature-set is associated to a coressponding element in :dataset
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
                (:shift-weights-strategy . :shift-weights-by-single-dimension)
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
                (:exp-top-dir . "cxn-nlp")  ;; directory name that groups a set of related of experiments together (e.g. logging/train/<exp-top-dir>)
                (:exp-name . "clevr-simulated")     ;; directory name that groups different runs of an experiment (i.e. logging/train/<exp-top-dir>/<exp-name>)
                (:log-dir-name . "phase-1") ;; directory name for a single run (i.e. logging/train/<exp-top-dir>/<exp-name>/<log-dir-name>)
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
                         "record-construction-inventory-size"
                         "export-communicative-success"
                         "export-construction-inventory-size"
                         )
                   (list "train" "val")
                   *experiment*)

  (format t "~% --> Running a new serie of interactions.~%")
  (let ((nr-of-interactions 5000))
    (set-configuration *experiment* :nr-of-interactions nr-of-interactions)
    (run-interaction *experiment*)
    (while (< (interaction-number (current-interaction *experiment*)) nr-of-interactions)
      do (run-interaction *experiment*)))

  (notify run-series-finished *experiment*)
  (notify series-finished 1)
  (notify batch-finished (class-string *experiment*))
  )

(defun display-success ()
  (loop for key being the hash-keys of *success-per-concept*
        using (hash-value counts)
        for num = (car counts)
        for denom = (cdr counts)
        do (format t "~%~,1f (~a)"
                   (* 100 (/ num denom))
                   key)))

(display-success)


(ql:quickload :concept-representations)
(in-package :concept-representations)

(defun make-weighted-distribution (prototype)
  (let* ((distribution (make-instance 'gaussian
                                      :mean (cle::mean (cle::distribution prototype))
                                      :st-dev (cle::st-dev (cle::distribution prototype))
                                      :nr-of-samples (cle::nr-of-samples (cle::distribution prototype))
                                      :M2 (cle::M2 (cle::distribution prototype))))
         (weighted-distribution (make-instance 'weighted-distribution
                                               :feature-name (cle::channel prototype)
                                               :weight-value (cle::weight-val prototype)
                                               :distribution distribution)))
    weighted-distribution))

(defmethod create-concept ((cxn cle::cxn))
  "Create a concept with a multivariate distribution representation for the given entity."
  (let ((representation (loop with weighted-distributions = (make-hash-table :test #'eq)
                              for feature-name being the hash-keys of (cle::prototypes (cle::meaning cxn))
                                using (hash-value prototype)
                              ;; create a weighted-distribution 
                              for weighted-distribution = (make-weighted-distribution prototype)
                              do (setf (gethash feature-name weighted-distributions) weighted-distribution)
                              finally (return weighted-distributions))))
    ;; create the concept
    (make-instance 'weighted-multivariate-distribution-concept :representation representation)))

(defun store-lexicon (agent path)
  (let ((inventory (loop with inventory = (make-hash-table :test #'equalp)
                         for key being the hash-keys of (cle::get-inventory (cle::lexicon agent) :fast)
                           using (hash-value cxn)
                         for new-concept = (create-concept cxn)
                         do (setf (gethash (cle::form cxn) inventory) new-concept)
                         finally (return inventory))))
    (ensure-directories-exist path)
    (cl-store:store inventory path)))


(store-lexicon (second (cle::agents cle::*experiment*))
               (babel-pathname
                :directory `("experiments" 
                             "concept-emergence2" 
                             "logging"
                             ,(get-configuration cle::*experiment* :exp-top-dir)
                             "train"
                             ,(get-configuration cle::*experiment* :exp-name)
                             ,(get-configuration cle::*experiment* :log-dir-name))
                :name (format nil "inventory")
                :type "store"))




;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; display the lexicon of an agent
(cle::display-lexicon (second (cle::agents cle::*experiment*)) :weight-threshold 0.1 :sort t)
