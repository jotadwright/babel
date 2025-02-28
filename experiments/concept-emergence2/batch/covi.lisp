(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

;; (progn
;;   (defparameter *concept-learning-game*
;;     (make-configuration
;;      :entries `(
;;                 ;; monitoring
;;                 (:log-every-x-interactions . 1000) ;; integer, frequence of when to log measures to standard output
;;                 (:usage-table-window . 100) ;; integer, window size of the construction inventory usage table
;;                 (:save-distribution-history . nil) ;; t or nil, whether to save the history of updates to the distribution (very memory-intensive!)
;;                 ;; setup environment
;;                 (:dataset-loader . :precomputed) ;; :precomputed or :runtime, load data in by scene (:precomputed) or by objects (:runtime)
;;                 (:dataset-view . :shared-views) ;; :shared-views or :exclusive-views, all views are shared or is each agent assigned a view?
;;                 (:dataset "clevr") ;; list of strings, each string represents a view over a dataset
;;                 (:feature-set "clevr") ;; list of strings, each string represents a feature set (stored in Corpora/concept-emergence2/-feature-sets), every feature-set is associated to a coressponding element in :dataset
;;                 (:dataset-split . "train") ;; string, "train" or "test", which split of the data to use?
;;                 ;; setup game
;;                 (:interacting-agents-strategy . :standard) ;; :standard [AT THE MOMENT, ONLY OPTION AVAILABLE]
;;                 (:population-size . 10) ;; integer, size of the population
;;                 (:min-context-size . 10) ;; integer, minimum number of context elements
;;                 (:max-context-size . 10) ;; integer, maximum number of context elements
;;                 ;; disable channels
;;                 (:disable-channels . :none) ;; :none, :random. :fixed
;;                 (:amount-disabled-channels . 0) ;; integer, amount of channels to disable
;;                 ;; noised channels
;;                 (:sensor-noise . :none) ;; :none or :shift
;;                 (:sensor-std . 0.0) ;; float, corresponds to calibration noise
;;                 (:observation-noise . :none) ;; :none or :shift
;;                 (:observation-std . 0.0) ;; float, noise during perception
;;                 ;; scene sampling
;;                 (:scene-sampling . :random) ;; :random [AT THE MOMENT, ONLY OPTION AVAILABLE]
;;                 (:topic-sampling . :random) ;; :random [AT THE MOMENT, ONLY OPTION AVAILABLE]
;;                 ;; alignment
;;                 (:initial-cxn-entrenchement . 0.5) ;; default entrenchment score
;;                 (:align . t) ;; t or nil, activates alignment or not
;;                 (:entrenchment-incf . 0.1) ;; hyperparameter for alignment (:align)
;;                 (:entrenchment-decf . -0.1) ;; hyperparameter for alignment (:align)
;;                 (:entrenchment-li . -0.02) ;; lateral inhibition, hyperparameter for alignment (:align)
;;                 ;; concept representation parameters
;;                 (:M2 . 0.0001) ;; float, default initialisation for gaussian distributions
;;                 ;; prototype weight inits
;;                 (:weight-update-strategy . :j-interpolation) ;; :standard or :j-interpolation
;;                 (:initial-weight . 0) ;; default weight
;;                 (:weight-incf . 1)    ;; :standard uses floats, j-interpolation uses int
;;                 (:weight-decf . -5)   ;; :standard uses floats, j-interpolation uses int
;;                 ;; experimental alternatives
;;                 (:prototype-distance . :paper) ;; :paper or :paper-wo-ledger
;;                 ;; staging
;;                 (:switch-condition . :none) ; :none, :after-n-interactions
;;                 (:switch-conditions-after-n-interactions . 50000) ;;
;;                 (:stage-parameters ((:switch-disable-channels-half . 10))) ;;
;;                 ;; measures
;;                 (:coherence-perspective . :hearer) ;; :hearer or :speaker, determines how conventionalisation is measured
;;                 ;; paths for exporting data to disk
;;                 (:exp-top-dir . "a")  ;; directory name that groups a set of related of experiments together (e.g. logging/train/<exp-top-dir>)
;;                 (:exp-name . "b")     ;; directory name that groups different runs of an experiment (i.e. logging/train/<exp-top-dir>/<exp-name>)
;;                 (:log-dir-name . "c") ;; directory name for a single run (i.e. logging/train/<exp-top-dir>/<exp-name>/<log-dir-name>)
;;                 )))
;;   (setf *experiment* (make-instance 'cle-experiment :configuration *concept-learning-game*))
;;   (notify reset-monitors)
;;   (wi::reset))

(defun parse-config (args)
  (let ((config (loop for (a b) on args by #'cddr
                      collect (cons (parse-keyword a) (read-from-string b)))))
    ;; make sure that the keys required in paths are lowercase
    (loop for (key . val) in config
          when (find key (list :exp-name :dataset-split :exp-top-dir))
            do (rplacd (assoc key config)
                       (string-downcase (string (assqv key config)))))
    ;; lists of strings should also be downcase
    (loop for (key . val) in config
          when (find key (list :dataset :feature-set))
          ;; loop through strings in val and downcase theme
          do (rplacd (assoc key config)
                     (mapcar #'string-downcase val)))
    (when (assoc :stage-parameters config)
      (let ((stage-params (assqv :stage-parameters config)))
        (loop for stage-param in stage-params
              do (when (assoc :switch-dataset stage-param)
                   (rplacd (assoc :switch-dataset stage-param)
                           (string-downcase (string (assqv :switch-dataset stage-param)))))
              do (when (assoc :switch-dataset-split stage-param)
                   (rplacd (assoc :switch-dataset-split stage-param)
                           (string-downcase (string (assqv :switch-dataset-split stage-param)))))
              do (when (assoc :switch-feature-set stage-param)
                   (rplacd (assoc :switch-feature-set stage-param)
                           (string-downcase (string (assqv :switch-feature-set stage-param))))))))
    config))

(defun fixed-config ()
  `(;; fixed in stone
    ;; --------------
    (:log-every-x-interactions . 5000)
    ;(:record-every-x-interactions . 100)
    (:usage-table-window . 5000)
    (:save-distribution-history . nil)
    (:interacting-agents-strategy . :standard)
    (:initial-cxn-entrenchement . 0.5)
    ;; parameter for updating continuous distributions (gaussian-welford)
    (:M2 . 0.0001)))

(defun run-experiment (args)
  (let* (;; parse command line arguments, append it to the fixed configuration
         (config (append (fixed-config) (parse-config args)))
         ;; generate a log-dir-name
         (log-dir-name (generate-log-dir-name (assqv :seed config))))
    ;; add log-dir-name to configuration
    (setf config (append config (list (cons :log-dir-name log-dir-name))))
    ;; adapt file-writing monitors so they output in the correct log-dir
    (set-up-monitors (list "export-communicative-success"
                           "export-conventionalisation"
                           "export-construction-inventory-usage-train"
                           "export-experiment-configurations"
                           "export-experiment-store"
                           "log-every-x-interactions-in-output-browser")
                     config)

    ;; Run experiment
    (format t "~%~% == Running the experiment, log at 'logging/~a/~a/~a'.~%"
            (assqv :exp-top-dir config)
            (assqv :exp-name config)
            (assqv :log-dir-name config))
    (time
     (loop with *configuration* = (make-configuration :entries config)
           with *experiment* = (make-instance 'cle-experiment :configuration *configuration*)
           with timestep-log2 =  (list
                                  1 2 3 4 5 6 7 8 9
                                  10     20     30     50     70
                                  100    200    300    500    700
                                  1000   2000   3000   5000   7000
                                  10000  20000  30000  50000  70000
                                  100000 200000 300000 500000 700000 1000000)
           with dirs = (list "experiments"
                             "concept-emergence2"
                             "storage"
                             "evolution"
                             (first (get-configuration *experiment* :dataset)))
           for i from 0 to 1000000
            ;when (eq (mod i timestep-log) 0)
           when (member i timestep-log2)
             do (progn
                  (format t "~%------> Logging ~a <---------~%" i)
                  (loop for agent in (agents *experiment*)
                        do (log-inventory dirs agent i :fast)
                        do (log-inventory dirs agent i :trash)))
           do (run-interaction *experiment*)))
    (format t "~%~% == Completed experiment.~%~%")))

(run-experiment #+sbcl (rest sb-ext:*posix-argv*))
(sb-ext:quit)