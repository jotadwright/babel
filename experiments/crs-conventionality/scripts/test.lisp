;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;;  Script for testing  ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the system and set *package*
(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;;      Naming game setting      ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;
;;       Canonical     ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  ;; reset the web interface
  (wi::reset)
  (notify reset-monitors)
  ;; deactivate all monitors (as a sanity check)
  (deactivate-all-monitors)
  ;; configure a canonical naming game
  (defparameter *configuration-canonical* (make-configuration
                                           :entries '(;; Logging
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
                                                      (:determine-topic-mode . :random-entity-from-scene))))
  ;; instantiate a naming game experiment
  (defparameter *naming-game-canonical* (make-instance 'naming-game-experiment
                                                       :configuration *configuration-canonical*))
  ;; activate monitors
  (activate-monitor log-every-x-interactions-in-output-browser))



;; Option 1: run experiment with real-time plotting (using gnuplot)

(progn
  
  ;; activate recorders
  (activate-monitor record-communicative-success)
  (activate-monitor record-conventionalisation)
  (activate-monitor record-construction-inventory-size)
  ;; activate tracers
  ;(activate-monitor trace-interaction)
  ;(activate-monitor trace-fcg-crs)
  ;(activate-monitor trace-irl-crs)
  ;; activate the gnuplot live display
  (activate-monitor display-metrics)

  ;; run the experiment
  (loop for i from 1 to 1000
        do (run-interaction *naming-game-canonical*)))


;; Option 2: run experiment locally and export results to disk

(progn
  (activate-monitor export-communicative-success)
  (activate-monitor export-conventionalisation)
  (activate-monitor export-construction-inventory-size)
  ;; run the experiment
  (run-batch 'naming-game-experiment ;; experiment-class
             5000 ;; nr-of-interactions
             1 ;; nr-of-series
             :configuration *configuration-canonical*))





;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Learnability    ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;


(progn
  ;; reset the web interface
  (wi::reset)
  ;; deactivate all monitors (as a sanity check)
  (monitors::notify reset-monitors)
  ;; configure a canonical naming game
  (defparameter *configuration-canonical* (make-configuration
                                           :entries '(;; Logging
                                                      (:log-every-x-interactions . 100)
                                                      ;; Initialising the experiment
                                                      (:nr-of-entities-in-world . 10)
                                                      (:nr-of-agents-in-population . 10)
                                                      (:nr-of-entities-in-scene . 5)
                                                      (:alignment-strategy . :lateral-inhibition)
                                                      (:learning-strategy . :default)
                                                      (:learning-rate . 0.5)
                                                      ;; Initialising an interaction
                                                      (:determine-interacting-agents-mode . :random-from-population)
                                                      (:determine-scene-entities-mode . :random-subset-of-world)
                                                      (:determine-topic-mode . :random-entity-from-scene))))
  ;; instantiate a naming game experiment
  (defparameter *naming-game-learnability* (make-instance 'naming-game-experiment
                                                       :configuration *configuration-canonical*))
  ;; activate monitors
  (activate-monitor log-every-x-interactions-in-output-browser))



(progn
  ;; Activate recorders
  (activate-monitor record-communicative-success)
  (activate-monitor record-conventionalisation)
  (activate-monitor record-construction-inventory-size)
  ;; Activate tracers
  ;(activate-monitor trace-interaction)
  ;(activate-monitor trace-fcg)
  ;(activate-monitor trace-irl)
  ;; Activate the gnuplot live display
  (activate-monitor display-metrics)

  ;; run the experiment
  (loop for i from 1 to 3
        do (run-interaction *naming-game-learnability*)))


(progn
  (monitors::notify reset-monitors)
  ;; First throw in agents to the *naming-game-learnability* 
  (throw-in-new-agents *naming-game-learnability* :number-of-agents 4)

  ;; Change the configuration of the experiment to make sure that a 'new' agent is selected for the interaction as the listener. 
  (set-configuration *naming-game-learnability* :determine-interacting-agents-mode :random-listener-from-younger-generation)

  ;; Run the experiment
  (loop for i from 1 to 1000
        do (run-interaction *naming-game-learnability*))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;;   Concept emergence setting      ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(wi::reset-id-counters)

;; Concept emergence setting ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set dataset as a configuration
(progn
  (monitors::notify reset-monitors)
  
  (defparameter *configuration-canonical-concept-emergence* (make-configuration
                                                             :entries '(;; Logging
                                                                        (:log-every-x-interactions . 100)
                                                                        ;; Initialising the experiment
                                                                        (:dataset . "winery")
                                                                        (:datasplit . "train")
                                                                        (:nr-of-entities-in-world . 10)
                                                                        (:nr-of-agents-in-population . 5)
                                                                        (:nr-of-entities-in-scene . 5)
                                                                        (:alignment-strategy . :concept-alignment)
                                                                        (:learning-strategy . :default)
                                                                        (:learning-rate . 0.5)
                                                                        (:neighbor-q-value-lr . 0.01)
                                                                        ;; Initialising an interaction
                                                                        (:determine-interacting-agents-mode . :random-from-social-network)
                                                                        (:determine-scene-entities-mode . :random-subset-of-world)
                                                                        (:determine-topic-mode . :random-entity-from-scene)))))

(defparameter *concept-emergence-canonical* (make-instance 'concept-emergence-game-experiment
                                                           :configuration *configuration-canonical-concept-emergence*))



(progn
  ;(activate-monitor display-metrics)
  (activate-monitor log-every-x-interactions-in-output-browser)
  (activate-monitor display-metrics)
  
  (deactivate-monitor trace-interaction)
  (deactivate-monitor trace-fcg-crs)
  (deactivate-monitor trace-irl-crs)
  
    (loop for i from 1 to 1000
        do (run-interaction *concept-emergence-canonical*)))


(progn
  (deactivate-monitor display-metrics)
  (activate-monitor trace-interaction)
  (activate-monitor trace-fcg-crs)
  (activate-monitor trace-irl-crs)
  
    (loop for i from 1 to 10
        do (run-interaction *concept-emergence-canonical*)))




;(reset-id-counters)
;(deactivate-all-monitors)


;;    Old testing of FCG     ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (def-fcg-constructions test
    :hashed t
    :feature-types ((meaning set-of-predicates)
                    (form set)
                    (footprints set))
    :fcg-configurations (;; Rendering and de-rendering
                         (:de-render-mode . :de-render-raw)
                         (:render-mode . :render-raw)
                         (:create-initial-structure-mode . :topic-and-scene)
                         ;; Construction supplier and search
                         (:construction-inventory-processor-mode . :heuristic-search)
                         (:node-expansion-mode . :full-expansion)
                         (:cxn-supplier-mode . :hashed)
                         ;; for using heuristics
                         (:search-algorithm . :best-first)
                         (:heuristics :cxn-score)
                         (:heuristic-value-mode . :sum-heuristics-and-parent) 
                         ;; goal tests
                         (:parse-goal-tests :no-sequence-in-root)
                         (:production-goal-tests :topic-retrieved)
                         (:max-nr-of-nodes . 3))
    :visualization-configurations ((:hide-features . nil))

    (eval `(def-fcg-cxn bolima-cxn
                        ((?bolima-unit
                          (meaning ((bind naming-game-entity ?entity
                                          ,(make-instance 'naming-game-entity :id 'object-2 :world (world *naming-game-canonical*)))
                                    (retrieve-from-scene ?target-entity ?entity ?scene))))
                         <-
                         (root
                          (scene ?scene)
                          (topic ,(make-instance 'crs-conventionality-entity-set 
                                                 :id 'crs-conventionality-entity-set-1
                                                 :entities (list (make-instance 'naming-game-entity :id 'object-2 :world (world *naming-game-canonical*)))))
                          --
                          )
                         (?bolima-unit
                          --
                          (HASH form ("bolima"))))))
    )
  (set-data (blackboard *fcg-constructions*) :primitive-inventory *naming-game-primitives*)
  )


;(deactivate-all-monitors)
;(activate-monitor trace-fcg-debugging)
;(activate-monitor trace-irl)


 (formulate (topic (first (interactions *naming-game-canonical*)))
            :cxn-inventory *fcg-constructions*
            :agent (speaker (first (interactions *naming-game-canonical*)))
            :scene (scene (first (interactions *naming-game-canonical*))))
















