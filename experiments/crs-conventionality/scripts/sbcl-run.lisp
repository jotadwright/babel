(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)

(time
 (run-experiments-sequentially 'concept-emergence-game-experiment
                               :strategies `((concept-emergence-canonical
                                              (;; Logging
                                               (:log-every-x-interactions . 100)
                                               ;; Initialising the experiment
                                               (:dataset . "winery")
                                               (:datasplit . "train")
                                               (:nr-of-entities-in-world . 3988)
                                               (:nr-of-agents-in-population . 10)
                                               (:nr-of-entities-in-scene . 5)
                                               (:alignment-strategy . :concept-alignment)
                                               (:learning-strategy . :default)
                                               (:learning-rate . 0.5)
                                               (:neighbor-q-value-lr . 0.01)
                                               ;; Initialising an interaction
                                               (:determine-interacting-agents-mode . :random-from-social-network)
                                               (:determine-scene-entities-mode . :random-subset-of-world)
                                               (:determine-topic-mode . :random-entity-from-scene)))
                                             (concept-emergence-no-alignment
                                              (;; Logging
                                               (:log-every-x-interactions . 100)
                                               ;; Initialising the experiment
                                               (:dataset . "winery")
                                               (:datasplit . "train")
                                               (:nr-of-entities-in-world . 100)
                                               (:nr-of-agents-in-population . 10)
                                               (:nr-of-entities-in-scene . 5)
                                               (:alignment-strategy . :no-alignment)
                                               (:learning-strategy . :default)
                                               (:learning-rate . 0.5)
                                               (:neighbor-q-value-lr . 0.01)
                                               ;; Initialising an interaction
                                               (:determine-interacting-agents-mode . :random-from-social-network)
                                               (:determine-scene-entities-mode . :random-subset-of-world)
                                               (:determine-topic-mode . :random-entity-from-scene)))
                                             )
                               
                               :number-of-interactions 10000
                               :number-of-series 10
                               :monitors (list "log-every-x-interactions-in-output-browser"
                                               "export-communicative-success"
                                               "export-conventionalisation"
                                               "export-construction-inventory-size")))


