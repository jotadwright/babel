;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; All functionality for running the crs-conventionality experiment ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the system and set *package*
(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)

(run-experiments-sequentially 'naming-game-experiment
                    :strategies `(('run-1
                                    (;; Logging
                                    (:log-every-x-interactions . 100)
                                    ;; Initialising the experiment
                                    (:nr-of-entities-in-world . 5)
                                    (:nr-of-agents-in-population . 2)
                                    (:nr-of-entities-in-scene . 3)
                                    (:alignment-strategy . :lateral-inhibition)
                                    (:learning-strategy . :default)
                                    (:learning-rate . 0.5)
                                    ;; Initialising an interaction
                                    (:determine-interacting-agents-mode . :random-from-population)
                                    (:determine-scene-entities-mode . :random-subset-of-world)
                                    (:determine-topic-mode . :random-entity-from-scene))))
                    :number-of-interactions 5000
                    :number-of-series 2
                    :monitors (list "print-a-dot-for-each-interaction"
                                    "export-communicative-success"
                                    "export-conventionalisation"))

;; PARALLEL BATCHES TODO
;; (run-parallel-batch-for-different-configurations :asdf-system "crs-conventionality"
;;                                                  :package "crs-conventionality"
;;                                                  :experiment-class "naming-game-experiment"
;;                                                  :number-of-interactions 500
;;                                                  :number-of-series 1
;;                                                  :monitors (list "print-a-dot-for-each-interaction"
;;                                                                  "export-communicative-success"
;;                                                                  "export-conventionalisation")
;;                                                  :configurations '(
;;                                                                    (ng-pop2
;;                                                                     ((:nr-of-agents-in-population . 2)))
;;                                                                    (ng-pop10
;;                                                                     ((:nr-of-agents-in-population . 10)))

                     
;;                                                                    )
;;                                                  :shared-configuration `(;; Logging
;;                                                                          (:dot-interval . 100)
;;                                                                          ;; Initialising the experiment
;;                                                                          (:nr-of-entities-in-world . 5)
;;                                                                          ;(:nr-of-agents-in-population . 2)
;;                                                                          (:nr-of-entities-in-scene . 3)
;;                                                                          (:alignment-strategy . :lateral-inhibition)
;;                                                                          (:learning-strategy . :default)
;;                                                                          (:learning-rate . 0.5)
;;                                                                          ;; Initialising an interaction
;;                                                                          (:determine-interacting-agents-mode . :random-from-population)
;;                                                                          (:determine-scene-entities-mode . :random-subset-of-world)
;;                                                                          (:determine-topic-mode . :random-entity-from-scene))
;;                                                  :output-dir (babel-pathname :directory '("experiments" "crs-conventionality" "raw-data"))
;;                                                  :heap-size 12288)