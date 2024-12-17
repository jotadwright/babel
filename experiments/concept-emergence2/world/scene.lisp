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
  (loop with scene = (random-scene (world experiment))
        for agent in (interacting-agents experiment)
        do (set-data agent 'context scene)))

(defmethod random-scene ((world precomputed-world))
  "Load a random scene by fpath into memory."
  (let* ((fpath (random-elt (fpaths world)))
         (s-expr (decode-json-as-alist-from-source fpath))
         (scene (s-expr->cle-scene s-expr world)))
    (setf (current-scene world) scene)
    scene))

(defmethod random-scene ((world runtime-world))
  "Create a scene by randomly sampling objects"
  (let* ((context-size (sample-context-size world))
         (objects (random-elts (objects world) context-size))
         (scene (objects->cle-scene objects world)))
    (setf (current-scene world) scene)
    (current-scene world)))

(defun sample-context-size (world)
  (let* ((min-context-size (min-context-size world))
         (max-context-size (max-context-size world))
         (context-size (random-from-range min-context-size max-context-size)))
    context-size))