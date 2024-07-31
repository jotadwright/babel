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
       :number-of-interactions 10000000
       :number-of-series 5
       :monitors (list "export-communicative-success"
                       "export-lexicon-coherence"
                       "export-experiment-configurations"
                       "export-experiment-store"
                       "print-a-dot-for-each-interaction"
                       )
       ;; default configuration settings
       :shared-configuration `(
                ;; monitoring
                (:dot-interval . 1000)
                (:save-distribution-history . nil)
                ;; setup interacting agents
                (:interacting-agents-strategy . :standard)
                ;(:population-size . 10)
                ;; setup data scene
                (:dataset . "clevr-extracted")
                (:dataset-split . "val")
                (:data-fname . "all.lisp")
                (:available-channels ,@(get-all-channels :clevr-extracted))
                ;; disable channels
                (:disable-channels . :none)
                (:amount-disabled-channels . 0)
                ;; noised channels
                (:sensor-noise . :none)
                (:sensor-std . 0.0)
                (:observation-noise . :none)
                (:observation-std . 0.0)
                ;; scene sampling
                (:scene-sampling . :deterministic)
                (:topic-sampling . :discriminative)

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
                )
       ;; configurations
       :configurations `(
                         (:population-size 100 1000 10)
                         )
       ;; output directory
       :output-dir (babel-pathname :directory '("experiments" "concept-emergence2" "logging" "clevr-extracted-scale-agents"))
       :heap-size 12248))

#|
(calculate-amount-of-variations `(;(:similarity-threshold 0.0 0.01 0.05 0.1 0.2)
                                  (:initial-weight 0 35)
                                  ))
|#