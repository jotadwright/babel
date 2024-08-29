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
  "Cycles through a list of scenes."
  (loop with scene = (get-next-scene experiment)
        for agent in (interacting-agents experiment)
        do (set-data agent 'context scene)))

;; ------------------
;; + Loading scenes +
;; ------------------
(defmethod load-scene (fpath feature-set)
  "Load a scene from filepath."
  (let* ((dataset (sixth (pathname-directory fpath)))
         (split (eighth (pathname-directory fpath)))
         (s-expr (decode-json-as-alist-from-source fpath)))
    (s-expr->cle-scene s-expr
                       :dataset dataset
                       :dataset-split split
                       :feature-set feature-set)))

(defmethod random-scene ((world world))
  "Choose a random scene and load it into memory."
  (setf (current-scene world) (load-scene (random-elt (scene-fpaths world))
                                          (feature-set world)))
  (current-scene world))

(defun get-next-scene (experiment)
  "Gets the next scene and updates the current index."
  (let ((scene-ids (get-configuration experiment :scene-ids))
        (current-idx (get-configuration experiment :current-scene-idx)))
    (set-configuration experiment :current-scene-idx (mod (+ current-idx 1) (length scene-ids)))
    (get-scene-by-index (world experiment) (nth current-idx scene-ids))))

(defmethod get-scene-by-index ((world world) index)
  "Get a particular scene by its index."
  (assert (and (>= index 0) (< index (length (scene-fpaths world)))))
  (setf (current-scene world)
        (load-scene (nth index (scene-fpaths world))
                    (feature-set world)))
  (current-scene world))
