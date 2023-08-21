(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(deactivate-all-monitors)

;; --------------------------------
;; + Batch experiments on CLUSTER +
;; --------------------------------


(time (run-parallel-batch-for-grid-search
       :asdf-system "cle"
       :package "cle"
       :experiment-class "cle-experiment"
       :number-of-interactions 1300
       :number-of-series 3
       :monitors (list "export-communicative-success"
                       "export-lexicon-coherence"
                       "export-experiment-configurations"
                       "export-experiment-store"
                       "print-a-dot-for-each-interaction"
                       )
       ;; default configuration settings
       :shared-configuration `(
                ;; monitoring
                (:dot-interval . 100)
                (:save-distribution-history . nil)
                ;; setup interacting agents
                (:interacting-agents-strategy . :standard)
                (:population-size . 10)
                ;; setup data scene
                (:dataset . "clevr")
                (:dataset-split . "train")
                ;(:data-fname . "all.lisp")
                (:available-channels ,@(get-all-channels :all))
                ;; disable channels
                (:disable-channels . :none)
                (:amount-disabled-channels . 0)
                ;; noised channels
                (:sensor-noise . :none)
                (:sensor-std . 0.0)
                (:observation-noise . :none)
                (:observation-std . 0.0)
                ;; scene sampling
                (:scene-sampling . :random)
                (:topic-sampling . :discriminative)
                ;; general strategy
                (:align . t)
                (:similarity-threshold . 0.0)
                ;; entrenchment of constructions
                (:initial-cxn-entrenchement . 1/2)
                (:entrenchment-incf . 1/10)
                (:entrenchment-decf . -1/10)
                (:entrenchment-li . -1/50) ;; lateral inhibition
                (:trash-concepts . nil)
                ;; concept representations
                (:concept-representation . :distribution)
                (:distribution . :gaussian-welford)
                (:M2 . 0.0001) ;; only for gaussian-welford
                ;; prototype weight inits
                (:weight-update-strategy . :j-interpolation)
                (:initial-weight . 0)
                (:weight-incf . 1)
                (:weight-decf . -1)
                ;; staging
                (:switch-condition . :after-n-interactions) ; :after-n-interactions)
                (:switch-conditions-after-n-interactions . 500)
                (:stage-parameters
                 ((:switch-disable-channels ,'area ,'bb-area))
                 ((:switch-disable-channels ,'area ,'bb-area))
                 )
                )
       ;; configurations
       :configurations `(;(:similarity-threshold 0.0 0.01 0.05 0.1 0.2)
                         ;(:initial-weight 0 35)
                         )
       ;; output directory
       :output-dir (babel-pathname :directory '("experiments" "concept-emergence2" "logging" "all"))
       :heap-size 12248))

#|
(calculate-amount-of-variations `(;(:similarity-threshold 0.0 0.01 0.05 0.1 0.2)
                                  (:initial-weight 0 35)
                                  ))
|#