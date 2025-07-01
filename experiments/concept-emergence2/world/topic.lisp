(in-package :cle)

;; ------------------
;; + Topic sampling +
;; ------------------
(defgeneric sample-topic (experiment mode)
  (:documentation "Sample and assign a topic from the chosen scene."))

(defmethod sample-topic (experiment (mode (eql :random)))
  "Any object can serve as topic."
  (let* ((agent-1 (first (interacting-agents experiment)))
         (agent-2 (second (interacting-agents experiment)))
         (cle-topic-1 (random-elt (entities (find-data agent-1 'context))))
         (cle-topic-2
          (if (concept-representations::get-entity-id cle-topic-1)
            (concept-representations::find-entity-by-id cle-topic-1
                                                        (entities (find-data agent-2 'context)))
            (concept-representations::find-entity-by-equality cle-topic-1
                                                              (entities (find-data agent-2 'context))))))
    (set-data agent-1 'topic cle-topic-1)
    (set-data agent-2 'topic cle-topic-2)))

;; (defmethod sample-topic (experiment (mode (eql :discriminative)))
;;   "Only entities that can be distinguished using a single metadata dimension can serve as topic.

;;    Assumes that the dataset contains symbolic annotations of the data.
;;    Topic strategy corresponding to the Frontiers paper by Nevens et al. (2020)."
;;   (let* ((interaction (current-interaction experiment))
;;          (agent (first (interacting-agents experiment)))
;;          (cle-scene (find-data agent 'context))
;;          (world (world experiment))
;;          (candidate-topics (filter-discriminative-topics world (entities cle-scene))))
;;     (if candidate-topics
;;       (loop with cle-topic = (random-elt candidate-topics)
;;             for agent in (interacting-agents experiment)
;;             do (set-data agent 'topic cle-topic))
;;       (progn
;;         (sample-scene experiment (get-configuration experiment :scene-sampling))
;;         (sample-topic experiment (get-configuration experiment :topic-sampling))))))

;; --------------------
;; + Helper functions +
;; --------------------

;; (defun filter-discriminative-topics (world context &key attribute)
;;   "Determines which entities in the context are discriminative."
;;   (loop for entity in context
;;         when (is-discriminative world entity (remove entity context) :attribute attribute)
;;         collect entity))

;; (defun is-discriminative (world entity other-entities &key (attribute nil))
;;   "Checks if the entity has a single feature dimension that is different from all other entities."
;;   (loop for attr being the hash-keys of (description entity) 
;;         using (hash-value val)
;;         do (when (is-feature-available world attr (features entity))
;;              (let ((discriminative (cond ((and attribute (not (eq attr attribute))) ;; eq?
;;                                           nil)
;;                                          (t
;;                                           (loop for other-entity in other-entities
;;                                                 for other-val = (gethash attr (description other-entity))
;;                                                 always (not (equal val other-val)))))))
;;                (when discriminative
;;                  (return t))))))

;; ;; helper function for test.lisp
;; (defun get-symbolic-discriminative-feature (world topic context)
;;   "Returns which symbolic features of the topic are discriminative."
;;   (let ((other-entities (remove topic (entities context))))
;;     (loop for attr being the hash-keys of (description topic) 
;;           using (hash-value val)
;;           for available = (is-feature-available world attr (features topic))
;;           for discriminative = (loop for other-entity in other-entities
;;                                      for other-val = (gethash attr (description other-entity))
;;                                      always (not (equal val other-val)))
;;           if (and available discriminative)
;;             collect (cons attr val))))
