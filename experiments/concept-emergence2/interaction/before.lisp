(in-package :cle)

;; ---------------------------------
;; + Setup agents, scene and topic +
;; ---------------------------------

;; events
(define-event event-context-determined (experiment cle-experiment))

(defmethod before-interaction ((experiment cle-experiment) &key scene topic agents)
  ;; set agents
  (if agents
    (set-agents experiment agents)
    (determine-interacting-agents experiment
                                (current-interaction experiment)
                                (get-configuration experiment :interacting-agents-strategy)))
  ;; reset agents
  (loop for agent in (interacting-agents experiment)
        do (clear-agent agent))

  ;; load a scene
  (if scene
    (set-scene experiment scene)
    (sample-scene experiment (get-configuration experiment :scene-sampling)))

  ;; pick a topic
  (if topic
    (set-topic experiment topic)
    (sample-topic experiment (get-configuration experiment :topic-sampling)))

  ;; notify
  (notify interaction-started experiment (current-interaction experiment) (interaction-number (current-interaction experiment)))
  (notify event-context-determined experiment))
