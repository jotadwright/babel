(in-package :cle)

;; ------------------------
;; + Set a scene manually +
;; ------------------------

(defmethod set-scene (experiment scene-id)
  "Set a scene manually."
  (error "Code needs update.")
  ;; (loop with world = (world experiment)
  ;;       with view-name = (first (view-names (world))) ;; use the first view by default
  ;;       with scene = (load-precomputed-scene world view-name scene-id)
  ;;       for agent in (interacting-agents experiment)
  ;;       do (setf (current-view agent) view-name)
  ;;       do (set-data agent 'context scene))
  )

;; -------------------------
;; + Random Scene sampling +
;; -------------------------

(defmethod sample-scene (experiment (mode (eql :random)))
  "Sample a random scene and assign to experiment."
  (assign-random-scene experiment (world experiment) (get-configuration experiment :dataset-view)))

;; ----------------------
;; + Precomputed scenes +
;; ----------------------

(defmethod assign-random-scene (experiment (world precomputed-world) (mode (eql :exclusive-views)))
  "If the assignment of views is exclusive, then each agent is associated with a single view."
  (loop with scene-id = nil
        for agent in (interacting-agents experiment)
        ;; views will have by definition only have 1 view-name 
        for view-name = (first (views agent))
        if (not scene-id)
          do (destructuring-bind (id . scene) (random-scene (world experiment) view-name)
               (setf scene-id id)
               (setf (current-view agent) view-name)
               (set-data agent 'context scene))
        else
          do (progn
               (setf (current-view agent) view-name)
               (let* ((scene (load-precomputed-scene world view-name scene-id)))
                 (set-data agent 'context scene)))))

(defmethod assign-random-scene (experiment (world precomputed-world) (mode (eql :shared-views)))
  "Experiments where agents can share the exact same view over a scene."
  (if (equalp (length (views (first (interacting-agents experiment)))) 1)
    ;; if agents have only one possible view, make it the same object
    (loop with selected-scene = nil
          for agent in (interacting-agents experiment)
          ;; views will have by definition only have 1 view-name 
          for view-name = (first (views agent))
          if (not selected-scene)
            do (destructuring-bind (id . scene) (random-scene (world experiment) view-name)
                 (setf selected-scene scene)
                 (setf (current-view agent) view-name)
                 (set-data agent 'context selected-scene))
          else
            do (progn
                 (setf (current-view agent) view-name)
                 (set-data agent 'context selected-scene)))
    ;; otherwise ensure to assign each agent with a mutually exclusive view
    (loop with scene-id = nil
          with assigned-view = nil
          for agent in (interacting-agents experiment)
          if (not assigned-view)
            do (progn
                 (setf assigned-view (random-elt (views agent)))
                 (destructuring-bind (id . scene) (random-scene (world experiment) assigned-view)
                   (setf scene-id id)
                   (setf (current-view agent) assigned-view)
                   (set-data agent 'context scene)))
          else
            do (progn
                 (setf (current-view agent) (random-elt (remove assigned-view (views agent))))
                 (let* ((scene (load-precomputed-scene world (current-view agent) scene-id)))
                   (set-data agent 'context scene))))))

;; -------------------------------
;; + Scenes generated at runtime +
;; -------------------------------

(defmethod assign-random-scene (experiment (world runtime-world) (mode (eql :exclusive-views)))
  (if (equalp (length (views (first (interacting-agents experiment)))) 1)
    (loop with context-size = (sample-context-size world)
          with idxs = (random-elts (loop for i from 0 to (- 100 1) collect i) context-size)
          for agent in (interacting-agents experiment)
          for view-name = (first (views agent))
          for objects = (loop for idx in idxs collect (nth idx (data (get-view world view-name))))
          for scene = (objects->cle-scene objects world view-name)
          do (setf (current-view agent) view-name)
          do (set-data agent 'context scene))
    (error "Not supported, cannot assign runtime scenes if agents can have multiple views.")))

(defmethod assign-random-scene (experiment (world runtime-world) (mode (eql :shared-views)))
  (if (equalp (length (views (first (interacting-agents experiment)))) 1)
    (loop with selected-scene = nil
          for agent in (interacting-agents experiment)
          for view-name = (first (views agent))
          if (not selected-scene)
            do (progn
                 (setf selected-scene (random-scene (world experiment) view-name))
                 (setf (current-view agent) view-name)
                 (set-data agent 'context selected-scene))
          else
            do (progn
                 (setf (current-view agent) view-name)
                 (set-data agent 'context selected-scene)))
    (error "Not supported, cannot assign runtime scenes if agents can have multiple views.")))

;; --------------------------
;; + Scenes for naming game +
;; --------------------------

(defmethod assign-random-scene (experiment (world naming-game-world) (mode (eql :shared-views)))
  (if (equalp (length (views (first (interacting-agents experiment)))) 1)
    (loop with selected-scene = nil
          for agent in (interacting-agents experiment)
          for view-name = (first (views agent))
          if (not selected-scene)
            do (progn
                 (setf selected-scene (random-scene (world experiment) view-name))
                 (setf (current-view agent) view-name)
                 (set-data agent 'context selected-scene))
          else
            do (progn
                 (setf (current-view agent) view-name)
                 (set-data agent 'context selected-scene)))
    (error "Not supported, cannot assign runtime scenes if agents can have multiple views.")))