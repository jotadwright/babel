(in-package :cle)

;; ------------------------
;; + Set a scene manually +
;; ------------------------

(defmethod set-topic (experiment cle-object)
  "Set a topic manually."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-scene (find-data agent 'context))
         (cle-topic (find cle-object (objects cle-scene)
                          :test (lambda (x el) (equal (description x) (description el))))))
    (loop for agent in (interacting-agents experiment)
          do (set-data agent 'topic cle-topic))))

;; ------------------
;; + Topic sampling +
;; ------------------
(defgeneric sample-topic (experiment mode)
  (:documentation "Sample and assign a topic from the chosen scene."))

(defmethod sample-topic (experiment (mode (eql :random-topic)))
  "Any object can serve as topic."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-scene (find-data agent 'context))
         (cle-topic (random-elt (objects cle-scene))))
    (loop for agent in (interacting-agents experiment)
          do (set-data agent 'topic cle-topic))))

(defmethod sample-topic (experiment (mode (eql :deterministic)))
  "Force through config (current-topic-idx) which object is chosen as topic."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-scene (find-data agent 'context))
         (topic-ids (get-configuration experiment :topic-ids))
         (current-idx (get-configuration experiment :current-topic-idx))
         (cle-topic (nth current-idx (objects cle-scene))))
    (set-configuration experiment :current-topic-idx (mod (+ current-idx 1) (length topic-ids)))
    (loop for agent in (interacting-agents experiment)
          do (set-data agent 'topic cle-topic))))
