(in-package :cle)

;; ------------------------
;; + Set a scene manually +
;; ------------------------

#|(defmethod set-topic (experiment cle-object)
  "Set a topic manually."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-scene (find-data agent 'context))
         (cle-topic (find cle-object (objects cle-scene)
                          :test (lambda (x el) (equal (attributes x) (attributes el))))))
    (loop for agent in (interacting-agents experiment)
          do (set-data agent 'topic cle-topic))))|#

;; ------------------
;; + Topic sampling +
;; ------------------
(defgeneric sample-topic (experiment mode)
  (:documentation "Sample and assign a topic from the chosen scene."))

(defmethod sample-topic (experiment (mode (eql :random)))
  "Any object can serve as topic."
  (let* ((agent-1 (first (interacting-agents experiment)))
         (agent-2 (second (interacting-agents experiment)))
         (cle-topic-1 (random-elt (objects (find-data agent-1 'context))))
         (cle-topic-2
          (if (has-topic-id cle-topic-1)
            (find-topic-by-id cle-topic-1 (objects (find-data agent-2 'context)))
            (find-topic-by-equality cle-topic-1 (objects (find-data agent-2 'context))))))
    (set-data agent-1 'topic cle-topic-1)
    (set-data agent-2 'topic cle-topic-2)))

#|(defmethod sample-topic (experiment (mode (eql :deterministic)))
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

(defmethod sample-topic (experiment (mode (eql :discriminative)))
  "Only objects that can be distinguished using a single metadata dimension can serve as topic.

   Assumes that the dataset contains symbolic annotations of the data.
   Topic strategy corresponding to the Frontiers paper by Nevens et al. (2020)."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-scene (find-data agent 'context))
         (world (world experiment))
         (candidate-topics (filter-discriminative-topics world (objects cle-scene))))
    (if candidate-topics
      (loop with cle-topic = (random-elt candidate-topics)
            for agent in (interacting-agents experiment)
            do (set-data agent 'topic cle-topic))
      (progn
        (sample-scene experiment (get-configuration experiment :scene-sampling))
        (sample-topic experiment (get-configuration experiment :topic-sampling))))))

(defmethod sample-topic (experiment (mode (eql :discr-color)))
  "Only objects that can be distinguished using a single metadata dimension can serve as topic."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-scene (find-data agent 'context))
         (world (world experiment))
         (candidate-topics (filter-discriminative-topics world (objects cle-scene) :attribute :color)))
    (if candidate-topics
      (loop with cle-topic = (random-elt candidate-topics)
            for agent in (interacting-agents experiment)
            do (set-data agent 'topic cle-topic))
      (progn
        (sample-scene experiment (get-configuration experiment :scene-sampling))
        (sample-topic experiment (get-configuration experiment :topic-sampling))))))|#

;; --------------------
;; + Helper functions +
;; --------------------

(defun find-topic-by-equality (agent-1-topic agent-2-objects)
  (find agent-1-topic agent-2-objects :test #'equalp))

(defun find-topic-by-id (agent-1-topic agent-2-objects)
  (let ((id (gethash :id (description agent-1-topic))))
    (find id agent-2-objects :test #'equal :key (lambda (x) (gethash :id (description x))))))

#|(defun filter-discriminative-topics (world context &key attribute)
  "Determines which objects in the context are discriminative."
  (loop for object in context
        when (is-discriminative world object (remove object context) :attribute attribute)
        collect object))

(defun is-discriminative (world object other-objects &key (attribute nil))
  "Checks if the object has a single channel dimension that is different from all other objects."
  (loop for attr being the hash-keys of (description object) 
        using (hash-value val)
        do (when (is-channel-available world attr (attributes object))
             (let ((discriminative (cond ((and attribute (not (eq attr attribute)))
                                          nil)
                                         (t
                                          (loop for other-object in other-objects
                                                for other-val = (gethash attr (description other-object))
                                                always (not (equal val other-val)))))))
               (when discriminative
                 (return t))))))

;; helper function for test.lisp
(defun get-symbolic-discriminative-feature (world topic context)
  "Returns which symbolic features of the topic are discriminative."
  (let ((other-objects (remove topic (objects context))))
    (loop for attr being the hash-keys of (description topic) 
          using (hash-value val)
          for available = (is-channel-available world attr (attributes topic))
          for discriminative = (loop for other-object in other-objects
                                     for other-val = (gethash attr (description other-object))
                                     always (not (equal val other-val)))
          if (and available discriminative)
            collect (cons attr val))))|#
