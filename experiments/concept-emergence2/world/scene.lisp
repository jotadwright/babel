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
  (assert (and (>= index 0) (< index (length (data world)))))
  (let* ((fpath (nth index (data world)))
         (s-expr (decode-json-as-alist-from-source fpath))
         (scene (s-expr->cle-scene s-expr world)))
    scene))

;; ------------------
;; + Scene sampling +
;; ------------------
(defmethod assign-random-scene (experiment (mode (eql :exclusive-views)))
  (loop for agent in (interacting-agents experiment)
        for view-name = (first (views agent))
        for scene = (random-scene (world experiment) view-name)
        do (set-data agent 'context scene)))

(defmethod assign-random-scene (experiment (mode (eql :shared-views)))
  (if (equalp (length (views (first (interacting-agents)))) 1)
    (assign-random-scene experiment :exclusive-views)
    (error "Multi-view not implemented yet.")))
          


(defmethod sample-scene (experiment (mode (eql :random)))
  "Sample a random scene and assign to experiment."
  (assign-random-scene experiment (get-configuration experiment :dataset-view)))
  #|(loop with scene-chosen = nil
        with view-chosen = nil
        for agent in (interacting-agents experiment)
        for views = (views agent)
        if (<= (length views) 1)
          do (set-data agent 'context )
        else
          do (let ((view-name (loop for view in views
                                    if (not (equalp view view-chosen))
                                      do (return view))))
               (if (not scene-chosen)
                 (progn
                   (setf (current-view agent) view-name)
                   (setf scene-chosen (random-scene (world experiment) view-name)))
                 (progn
                   (setf (current-view agent) view-name)
                   (set-data agent 'context scene-chosen)))))|#
       
