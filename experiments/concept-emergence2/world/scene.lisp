(in-package :cle)

;; ------------------------
;; + Set a scene manually +
;; ------------------------

(defmethod set-scene (experiment scene-id)
  "Set a scene manually."
  (loop with scene = (get-scene-by-index (world experiment) scene-id)
        for agent in (interacting-agents experiment)
        do (set-data agent 'context scene)))

;; ------------------
;; + Scene sampling +
;; ------------------
(defmethod sample-scene (experiment (mode (eql :random)))
  "Sample a random scene and assign to experiment."
  (loop with scene = (random-scene (world experiment))
        for agent in (interacting-agents experiment)
        do (set-data agent 'context scene)))

(defmethod sample-scene (experiment (mode (eql :deterministic)))
  "Cycles through a list of scenes (specified by get-scene-by-indexes)."
  (loop with scene = (get-scene-by-indexes experiment)
        for agent in (interacting-agents experiment)
        do (set-data agent 'context scene)))

;; helper functions
(defun get-scene-by-indexes (experiment)
  (let ((scene-ids (get-configuration experiment :scene-ids))
        (current-idx (get-configuration experiment :current-scene-idx)))
    (set-configuration experiment :current-scene-idx (mod (+ current-idx 1) (length scene-ids)))
    (get-scene-by-index (world experiment) (nth current-idx scene-ids))))




