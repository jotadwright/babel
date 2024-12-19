(in-package :cle)

;; ------------------------
;; + Set a scene manually +
;; ------------------------

(defmethod set-scene (experiment scene-id)
  "Set a scene manually."
  (loop with scene = (get-scene-by-index (world experiment) scene-id)
        for agent in (interacting-agents experiment)
        do (set-data agent 'context scene)))

(defmethod get-scene-by-index ((world world) index)
  "Get a particular scene by its index."
  (assert (and (>= index 0) (< index (length (fpaths world)))))
  (let* ((fpath (nth index (fpaths world)))
         (s-expr (decode-json-as-alist-from-source fpath))
         (scene (s-expr->cle-scene s-expr world)))
    (setf (current-scene world) scene)
    (current-scene world)))

;; ------------------
;; + Scene sampling +
;; ------------------
(defmethod sample-scene (experiment (mode (eql :random)))
  "Sample a random scene and assign to experiment."
  (loop with scene-chosen = nil
        for agent in (interacting-agents experiment)
        for 
        with scene = (random-scene (world experiment))
        do (set-data agent 'context scene)))
