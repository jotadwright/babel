;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; All functionality for running the naming game base experiments   ;;
;; and naming game experimental interventions.                      ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the system and set *package*
(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Base experiments (Fig. 1) ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-experiments-sequentially 'naming-game-experiment
                    :strategies `((base-conventional
                                    (;; Logging
                                    (:log-every-x-interactions . 100)
                                    ;; Initialising the experiment
                                    (:nr-of-entities-in-world . 10)
                                    (:nr-of-agents-in-population . 10)
                                    (:nr-of-entities-in-scene . 5)
                                    (:alignment-strategy . :lateral-inhibition)
                                    (:learning-strategy . :default)
                                    (:learning-rate . 0.5)
                                    (:neighbor-q-value-lr . 0.01)
                                    ;; Initialising an interaction
                                    (:determine-interacting-agents-mode . :random-from-social-network)
                                    (:determine-scene-entities-mode . :random-subset-of-world)
                                    (:determine-topic-mode . :random-entity-from-scene)))
                                  (base-unconventional
                                    (;; Logging
                                    (:log-every-x-interactions . 100)
                                    ;; Initialising the experiment
                                    (:nr-of-entities-in-world . 10)
                                    (:nr-of-agents-in-population . 10)
                                    (:nr-of-entities-in-scene . 5)
                                    (:alignment-strategy . :no-alignment)
                                    (:learning-strategy . :default)
                                    (:learning-rate . 0.5)
                                    (:neighbor-q-value-lr . 0.01)
                                    ;; Initialising an interaction
                                    (:determine-interacting-agents-mode . :random-from-social-network)
                                    (:determine-scene-entities-mode . :random-subset-of-world)
                                    (:determine-topic-mode . :random-entity-from-scene))))
                    
                    :number-of-interactions 5000
                    :number-of-series 10
                    :monitors (list "log-every-x-interactions-in-output-browser"
                                    "export-communicative-success"
                                    "export-conventionalisation"
                                    "export-construction-inventory-size"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Noise experiments (Fig. 3) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-experiments-sequentially 'naming-game-experiment
                    :strategies `((noise-conventional-0.25
                                    (;; Logging
                                     (:log-every-x-interactions . 100)
                                     ;; Initialising the experiment
                                     (:nr-of-entities-in-world . 10)
                                     (:nr-of-agents-in-population . 10)
                                     (:nr-of-entities-in-scene . 5)
                                     (:alignment-strategy . :lateral-inhibition)
                                     (:learning-strategy . :default)
                                     (:learning-rate . 0.5)
                                     (:neighbor-q-value-lr . 0.01)
                                     (:introduce-noise-after-interaction . 5000)
                                     (:noise-level . 0.25)
                                     ;; Initialising an interaction
                                     (:determine-interacting-agents-mode . :random-from-social-network)
                                     (:determine-scene-entities-mode . :random-subset-of-world)
                                     (:determine-topic-mode . :random-entity-from-scene)))
                                  (noise-conventional-0.50
                                    (;; Logging
                                     (:log-every-x-interactions . 100)
                                     ;; Initialising the experiment
                                     (:nr-of-entities-in-world . 10)
                                     (:nr-of-agents-in-population . 10)
                                     (:nr-of-entities-in-scene . 5)
                                     (:alignment-strategy . :lateral-inhibition)
                                     (:learning-strategy . :default)
                                     (:learning-rate . 0.5)
                                     (:neighbor-q-value-lr . 0.01)
                                     (:introduce-noise-after-interaction . 5000)
                                     (:noise-level . 0.50)
                                     ;; Initialising an interaction
                                     (:determine-interacting-agents-mode . :random-from-social-network)
                                     (:determine-scene-entities-mode . :random-subset-of-world)
                                     (:determine-topic-mode . :random-entity-from-scene)))
                                  (noise-conventional-0.75
                                    (;; Logging
                                     (:log-every-x-interactions . 100)
                                     ;; Initialising the experiment
                                     (:nr-of-entities-in-world . 10)
                                     (:nr-of-agents-in-population . 10)
                                     (:nr-of-entities-in-scene . 5)
                                     (:alignment-strategy . :lateral-inhibition)
                                     (:learning-strategy . :default)
                                     (:learning-rate . 0.5)
                                     (:neighbor-q-value-lr . 0.01)
                                     (:introduce-noise-after-interaction . 5000)
                                     (:noise-level . 0.75)
                                     ;; Initialising an interaction
                                     (:determine-interacting-agents-mode . :random-from-social-network)
                                     (:determine-scene-entities-mode . :random-subset-of-world)
                                     (:determine-topic-mode . :random-entity-from-scene)))
                                  (noise-unconventional-0.25
                                    (;; Logging
                                     (:log-every-x-interactions . 100)
                                     ;; Initialising the experiment
                                     (:nr-of-entities-in-world . 10)
                                     (:nr-of-agents-in-population . 10)
                                     (:nr-of-entities-in-scene . 5)
                                     (:alignment-strategy . :no-alignment)
                                     (:learning-strategy . :default)
                                     (:learning-rate . 0.5)
                                     (:neighbor-q-value-lr . 0.01)
                                     (:introduce-noise-after-interaction . 5000)
                                     (:noise-level . 0.25)
                                     ;; Initialising an interaction
                                     (:determine-interacting-agents-mode . :random-from-social-network)
                                     (:determine-scene-entities-mode . :random-subset-of-world)
                                     (:determine-topic-mode . :random-entity-from-scene)))
                                  (noise-unconventional-0.50
                                    (;; Logging
                                     (:log-every-x-interactions . 100)
                                     ;; Initialising the experiment
                                     (:nr-of-entities-in-world . 10)
                                     (:nr-of-agents-in-population . 10)
                                     (:nr-of-entities-in-scene . 5)
                                     (:alignment-strategy . :no-alignment)
                                     (:learning-strategy . :default)
                                     (:learning-rate . 0.5)
                                     (:neighbor-q-value-lr . 0.01)
                                     (:introduce-noise-after-interaction . 5000)
                                     (:noise-level . 0.50)
                                     ;; Initialising an interaction
                                     (:determine-interacting-agents-mode . :random-from-social-network)
                                     (:determine-scene-entities-mode . :random-subset-of-world)
                                     (:determine-topic-mode . :random-entity-from-scene)))
                                  (noise-unconventional-0.75
                                    (;; Logging
                                     (:log-every-x-interactions . 100)
                                     ;; Initialising the experiment
                                     (:nr-of-entities-in-world . 10)
                                     (:nr-of-agents-in-population . 10)
                                     (:nr-of-entities-in-scene . 5)
                                     (:alignment-strategy . :no-alignment)
                                     (:learning-strategy . :default)
                                     (:learning-rate . 0.5)
                                     (:neighbor-q-value-lr . 0.01)
                                     (:introduce-noise-after-interaction . 5000)
                                     (:noise-level . 0.75)
                                     ;; Initialising an interaction
                                     (:determine-interacting-agents-mode . :random-from-social-network)
                                     (:determine-scene-entities-mode . :random-subset-of-world)
                                     (:determine-topic-mode . :random-entity-from-scene))))
                    
                    :number-of-interactions 10000
                    :number-of-series 10
                    :monitors (list "log-every-x-interactions-in-output-browser"
                                    "export-communicative-success"
                                    "export-conventionalisation"
                                    "export-construction-inventory-size"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Population turnover experiments (Fig. 5) ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(run-experiments-sequentially 'naming-game-experiment
                    :strategies `((population-turnover-conventional
                                    (;; Logging
                                    (:log-every-x-interactions . 100)
                                    ;; Initialising the experiment
                                    (:nr-of-entities-in-world . 10)
                                    (:nr-of-agents-in-population . 10)
                                    (:nr-of-entities-in-scene . 5)
                                    (:alignment-strategy . :lateral-inhibition)
                                    (:learning-strategy . :default)
                                    (:learning-rate . 0.5)
                                    (:neighbor-q-value-lr . 0.01)
                                    (:replace-agents-after-interaction . 5000)
                                    (:proportion-of-agents-to-replace . 0.5)
                                    ;; Initialising an interaction
                                    (:determine-interacting-agents-mode . :random-from-social-network)
                                    (:determine-scene-entities-mode . :random-subset-of-world)
                                    (:determine-topic-mode . :random-entity-from-scene)))
                                  (population-turnover-unconventional
                                    (;; Logging
                                    (:log-every-x-interactions . 100)
                                    ;; Initialising the experiment
                                    (:nr-of-entities-in-world . 10)
                                    (:nr-of-agents-in-population . 10)
                                    (:nr-of-entities-in-scene . 5)
                                    (:alignment-strategy . :no-alignment)
                                    (:learning-strategy . :default)
                                    (:learning-rate . 0.5)
                                    (:neighbor-q-value-lr . 0.01)
                                    (:replace-agents-after-interaction . 5000)
                                    (:proportion-of-agents-to-replace . 0.5)
                                    ;; Initialising an interaction
                                    (:determine-interacting-agents-mode . :random-from-social-network)
                                    (:determine-scene-entities-mode . :random-subset-of-world)
                                    (:determine-topic-mode . :random-entity-from-scene))))
                    
                    :number-of-interactions 10000
                    :number-of-series 10
                    :monitors (list "log-every-x-interactions-in-output-browser"
                                    "export-communicative-success"
                                    "export-conventionalisation"
                                    "export-construction-inventory-size"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Learner agent experiments (Fig. 7) ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(run-experiments-sequentially 'naming-game-experiment
                    :strategies `((learner-agent-conventional
                                    (;; Logging
                                    (:log-every-x-interactions . 100)
                                    ;; Initialising the experiment
                                    (:nr-of-entities-in-world . 10)
                                    (:nr-of-agents-in-population . 10)
                                    (:nr-of-entities-in-scene . 5)
                                    (:alignment-strategy . :lateral-inhibition)
                                    (:learning-strategy . :default)
                                    (:learning-rate . 0.5)
                                    (:neighbor-q-value-lr . 0.01)
                                    (:introduce-new-agents-after-interaction . 5000)
                                    (:nr-of-agents-to-introduce . 1)
                                    ;; Initialising an interaction
                                    (:determine-interacting-agents-mode . :random-from-social-network)
                                    (:determine-scene-entities-mode . :random-subset-of-world)
                                    (:determine-topic-mode . :random-entity-from-scene)))
                                  (learner-agent-unconventional
                                    (;; Logging
                                    (:log-every-x-interactions . 100)
                                    ;; Initialising the experiment
                                    (:nr-of-entities-in-world . 10)
                                    (:nr-of-agents-in-population . 10)
                                    (:nr-of-entities-in-scene . 5)
                                    (:alignment-strategy . :no-alignment)
                                    (:learning-strategy . :default)
                                    (:learning-rate . 0.5)
                                    (:neighbor-q-value-lr . 0.01)
                                    (:introduce-new-agents-after-interaction . 5000)
                                    (:nr-of-agents-to-introduce . 1)
                                    ;; Initialising an interaction
                                    (:determine-interacting-agents-mode . :random-from-social-network)
                                    (:determine-scene-entities-mode . :random-subset-of-world)
                                    (:determine-topic-mode . :random-entity-from-scene))))
                    
                    :number-of-interactions 10000
                    :number-of-series 10
                    :monitors (list "log-every-x-interactions-in-output-browser"
                                    "export-communicative-success"
                                    "export-conventionalisation"
                                    "export-construction-inventory-size"
                                    "export-communicative-success-of-new-agents"
                                    "export-conventionalisation-of-new-agents"
                                    "export-construction-inventory-size-of-new-agents"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Vocabulary limit experiments (Fig. 9) ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(run-experiments-sequentially 'naming-game-experiment
                    :strategies `((vocabulary-limit-conventional
                                    (;; Logging
                                    (:log-every-x-interactions . 100)
                                    ;; Initialising the experiment
                                    (:nr-of-entities-in-world . 10)
                                    (:nr-of-agents-in-population . 10)
                                    (:nr-of-entities-in-scene . 5)
                                    (:alignment-strategy . :lateral-inhibition)
                                    (:learning-strategy . :default)
                                    (:learning-rate . 0.5)
                                    (:neighbor-q-value-lr . 0.01)
                                    (:cognitive-economy . :replace)
                                    (:threshold-cognitive-economy . 15)
                                    ;; Initialising an interaction
                                    (:determine-interacting-agents-mode . :random-from-social-network)
                                    (:determine-scene-entities-mode . :random-subset-of-world)
                                    (:determine-topic-mode . :random-entity-from-scene)))
                                  (vocabulary-limit-unconventional
                                    (;; Logging
                                    (:log-every-x-interactions . 100)
                                    ;; Initialising the experiment
                                    (:nr-of-entities-in-world . 10)
                                    (:nr-of-agents-in-population . 10)
                                    (:nr-of-entities-in-scene . 5)
                                    (:alignment-strategy . :no-alignment)
                                    (:learning-strategy . :default)
                                    (:learning-rate . 0.5)
                                    (:neighbor-q-value-lr . 0.01)
                                    (:cognitive-economy . :replace)
                                    (:threshold-cognitive-economy . 15)
                                    ;; Initialising an interaction
                                    (:determine-interacting-agents-mode . :random-from-social-network)
                                    (:determine-scene-entities-mode . :random-subset-of-world)
                                    (:determine-topic-mode . :random-entity-from-scene))))
                    
                    :number-of-interactions 5000
                    :number-of-series 10
                    :monitors (list "log-every-x-interactions-in-output-browser"
                                    "export-communicative-success"
                                    "export-conventionalisation"
                                    "export-construction-inventory-size"))

