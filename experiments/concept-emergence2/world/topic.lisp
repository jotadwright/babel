(in-package :cle)

;; ------------------
;; + Topic sampling +
;; ------------------
(defgeneric sample-topic (experiment mode)
  (:documentation "Sample and assign a topic from the chosen scene."))

(defmethod sample-topic (experiment (mode (eql :random-topic)))
  "Any object can serve as topic."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-context (find-data agent 'context))
         (cle-topic (random-elt (objects cle-context))))
    (loop for agent in (interacting-agents experiment)
          do (set-data agent 'topic cle-topic))))

(defmethod sample-topic (experiment (mode (eql :deterministic)))
  "Force through config (current-topic-idx) which object is chosen as topic."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-context (find-data agent 'context))
         (topic-ids (get-configuration experiment :topic-ids))
         (current-idx (get-configuration experiment :current-topic-idx))
         (cle-topic (nth current-idx (objects cle-context))))
    (set-configuration experiment :current-topic-idx (mod (+ current-idx 1) (length topic-ids)))
    (loop for agent in (interacting-agents experiment)
          do (set-data agent 'topic cle-topic))))

(defmethod sample-topic (experiment (mode (eql :english-concepts)))
  "Only objects that can be distinguished using a single metadata dimension can serve as topic."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-context (find-data agent 'context))
         (candidate-topics (filter-discriminative-topics (objects cle-context))))
    (if candidate-topics
      (let ((cle-topic (random-elt candidate-topics)))
        (set-data interaction 'attribute-type (get-symbolic-discriminative-feature cle-topic cle-context))
        (loop for agent in (interacting-agents experiment)
              do (set-data agent 'topic cle-topic)))
      (progn
        (sample-scene experiment (get-configuration experiment :scene-sampling))
        (sample-topic experiment (get-configuration experiment :topic-sampling))))))

;; --------------------
;; + Helper functions +
;; --------------------
(defun get-symbolic-discriminative-feature (topic context)
  "Returns which symbolic features of the topic are discriminative."
  (let ((other-objects (remove topic (objects context))))
    (loop for (attr . val) in (description topic)
          for available = (is-attribute-available attr (attributes topic))
          for discriminative = (loop for other-object in other-objects
                                     for other-val = (assqv attr (description other-object))
                                     always (not (equal val other-val)))
          if (and available discriminative)
            collect (cons attr val))))

(defun filter-discriminative-topics (context)
  "Determines which objects in the context are discriminative."
  (loop for object in context
        when (is-discriminative object (remove object context))
        collect object))

(defun is-discriminative (object other-objects)
  "Checks if the object has a single attribute dimension that is different from all other objects."
  (loop for (attr . val) in (description object)
        do (when (is-attribute-available attr (attributes object))
             (let ((discriminative (loop for other-object in other-objects
                                         for other-val = (assqv attr (description other-object))
                                         always (not (equal val other-val)))))
               (when discriminative
                 (return t))))))

(defun is-attribute-available (symbolic-attribute raw-attributes)
  (let ((continuous-attributes (mapcar 'first raw-attributes)))
    (case symbolic-attribute
      (:COLOR (or (if (member 'R continuous-attributes) t nil)
                  (if (member 'G continuous-attributes) t nil)
                  (if (member 'B continuous-attributes) t nil)))
      (:SIZE (if (member 'AREA continuous-attributes) t nil))
      (:SHAPE (or (if (member 'NR-OF-CORNERS continuous-attributes) t nil)
                  (if (member 'NR-OF-SIDES continuous-attributes) t nil)
                  (if (member 'WH-RATIO continuous-attributes) t nil)))
      (:MATERIAL (if (member 'ROUGHNESS continuous-attributes) t nil))
      (:XPOS (or (if (member 'XPOS continuous-attributes) t nil)
                 (if (member 'YPOS continuous-attributes) t nil)))
      (:ZPOS (or (if (member 'ZPOS continuous-attributes) t nil)
                 (if (member 'YPOS continuous-attributes) t nil))))))
