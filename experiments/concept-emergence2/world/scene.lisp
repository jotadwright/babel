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
  (assert (and (>= index 0) (< index (length (scene-fpaths world)))))
  (setf (current-scene world)
        (load-scene (nth index (scene-fpaths world))
                    (feature-set world)))
  (current-scene world))

;; ------------------
;; + Scene sampling +
;; ------------------

(defmethod sample-scene (experiment (mode (eql :random)))
  "Sample a random scene and assign to experiment."
  (loop with scene = (random-scene (world experiment) (dataset-loader (world experiment)))
        for agent in (interacting-agents experiment)
        do (set-data agent 'context scene)))

;; ----------------------------
;; + Scene sampling by scenes +
;; ----------------------------

(defmethod random-scene ((world world) (mode (eql :split-by-scenes)))
  "Load a random scene by fpath into memory."
  (let* ((fpath (random-elt-ht (data world)))
         (s-expr (decode-json-as-alist-from-source fpath))
         (scene (s-expr->cle-scene s-expr
                                   :dataset (dataset-name world)
                                   :dataset-split (dataset-split world)
                                   :feature-set (feature-set world))))
    (setf (current-scene world) scene)
    scene))

;; -----------------------------
;; + Scene sampling by objects +
;; -----------------------------

(defun sample-context-size (world)
  (let* ((min-context-size (min-context-size world))
         (max-context-size (max-context-size world))
         (context-size (random-from-range min-context-size max-context-size)))
    context-size))

(defmethod random-scene ((world world) (mode (eql :split-by-objects)))
  "Create a scene by randomly sampling objects"
  (let* ((context-size (sample-context-size world))
         (objects (random-elts-ht (data world) context-size))
         (scene (objects->cle-scene objects
                                    :dataset (dataset-name world)
                                    :dataset-split (dataset-split world)
                                    :feature-set (feature-set world))))
    (setf (current-scene world) scene)
    (current-scene world)))
