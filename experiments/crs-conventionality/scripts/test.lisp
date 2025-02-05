;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;; Script for running a quick demo of the crs-conventionality experiments ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the system and set *package*
(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)


;; Canonical naming game setting ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  ;; reset the web interface
  (wi::reset)
  ;; deactivate all monitors (as a sanity check)
  (deactivate-all-monitors)
  ;; configure a canonical naming game
  (defparameter *configuration-canonical* (make-configuration
                                           :entries '(;; Logging
                                                      (:log-every-x-interactions . 100)
                                                      ;; Initialising the experiment
                                                      (:nr-of-entities-in-world . 5)
                                                      (:nr-of-agents-in-population . 10)
                                                      (:nr-of-entities-in-scene . 4)
                                                      (:alignment-strategy . :lateral-inhibition)
                                                      (:learning-strategy . :default)
                                                      (:learning-rate . 0.5)
                                                      ;; Initialising an interaction
                                                      (:determine-interacting-agents-mode . :random-from-population)
                                                      (:determine-scene-entities-mode . :random-subset-of-world)
                                                      (:determine-topic-mode . :random-entity-from-scene))))
  ;; instantiate a naming game experiment
  (defparameter *naming-game-canonical* (make-instance 'naming-game-experiment
                                                       :configuration *configuration-canonical*))
  ;; activate monitors
  (activate-monitor log-every-x-interactions-in-output-browser))


;; Option 1: run experiment with real-time plotting (using gnuplot)

(progn
  ;(notify reset-monitors)
  ;; activate recorders
  (activate-monitor record-communicative-success)
  (activate-monitor record-conventionalisation)
  (activate-monitor record-construction-inventory-size)
  ;; activate tracers
  ;(activate-monitor trace-interaction)
  ;(activate-monitor trace-fcg)
  ;(activate-monitor trace-irl)
  ;; activate the gnuplot live display
  (activate-monitor display-metrics)

  ;; run the experiment
  (loop for i from 1 to 5000
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


;; Concept emergence setting ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *concept-emergence-canonical* (make-instance 'concept-emergence-experiment
                                                           :configuration *configuration-canonical*))

(loop for i from 1 to 100
      do (run-interaction *concept-emergence-canonical*))


;; Learnability setting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; ...

; (activate-monitor trace-fcg)
; (activate-monitor trace-irl)


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
















