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
                          :test (lambda (x el) (equal (attributes x) (attributes el))))))
    (loop for agent in (interacting-agents experiment)
          do (set-data agent 'topic cle-topic))))

;; ------------------
;; + Topic sampling +
;; ------------------
(defgeneric sample-topic (experiment mode)
  (:documentation "Sample and assign a topic from the chosen scene."))

(defmethod sample-topic (experiment (mode (eql :random)))
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

(defmethod sample-topic (experiment (mode (eql :discriminative)))
  "Only objects that can be distinguished using a single metadata dimension can serve as topic.

   Assumes that the dataset contains symbolic annotations of the data.
   Topic strategy corresponding to the Frontiers paper by Nevens et al. (2020)."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-scene (find-data agent 'context))
         (dataset (parse-keyword (get-configuration experiment :dataset)))
         (candidate-topics (filter-discriminative-topics dataset (objects cle-scene))))
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
         (dataset (parse-keyword (get-configuration experiment :dataset)))
         (candidate-topics (filter-discriminative-topics dataset (objects cle-scene) :attribute :color)))
    (if candidate-topics
      (loop with cle-topic = (random-elt candidate-topics)
            for agent in (interacting-agents experiment)
            do (set-data agent 'topic cle-topic))
      (progn
        (sample-scene experiment (get-configuration experiment :scene-sampling))
        (sample-topic experiment (get-configuration experiment :topic-sampling))))))

;; --------------------
;; + Helper functions +
;; --------------------
(defun filter-discriminative-topics (dataset context &key attribute)
  "Determines which objects in the context are discriminative."
  (loop for object in context
        when (is-discriminative dataset object (remove object context) :attribute attribute)
        collect object))

(defun is-discriminative (dataset object other-objects &key (attribute nil))
  "Checks if the object has a single channel dimension that is different from all other objects."
  (loop for (attr . val) in (description object)
        
        do (when (is-channel-available dataset attr (attributes object))
             (let ((discriminative (cond ((and attribute (not (eq attr attribute)))
                                          nil)
                                         (t
                                          (loop for other-object in other-objects
                                                for other-val = (assqv attr (description other-object))
                                                always (not (equal val other-val)))))))
               (when discriminative
                 (return t))))))

;; helper function for test.lisp
(defun get-symbolic-discriminative-feature (dataset topic context)
  "Returns which symbolic features of the topic are discriminative."
  (let ((other-objects (remove topic (objects context))))
    (loop for (attr . val) in (description topic)
          for available = (is-channel-available dataset attr (attributes topic))
          for discriminative = (loop for other-object in other-objects
                                     for other-val = (assqv attr (description other-object))
                                     always (not (equal val other-val)))
          if (and available discriminative)
            collect (cons attr val))))
