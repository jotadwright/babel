(in-package :cle)

;; ---------------------------------
;; + Setup agents, scene and topic +
;; ---------------------------------

(defmethod before-interaction ((experiment cle-experiment) &key scene agents)
  ;; 1. reset agents
  (determine-interacting-agents experiment
                                  (current-interaction experiment)
                                  (get-configuration experiment :interacting-agents-strategy)
                                  :agents agents)
  (loop for agent in (interacting-agents experiment)
        do (clear-agent agent))
  ;; 2. load a scene
  (if scene
    (set-scene experiment scene)
    (sample-scene experiment (get-configuration experiment :scene-sampling)))
  ;; 3. pick a topic
  (sample-topic experiment (get-configuration experiment :topic-sampling)))
