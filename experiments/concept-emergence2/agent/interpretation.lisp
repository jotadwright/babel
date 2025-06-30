(in-package :cle)

;; ------------------
;; + Interpretation +
;; ------------------

;; events
(define-event event-interpretation-end (agent cle-agent))

(defmethod interpret ((agent cle-agent))
  "Computes the weighted similarity between
     1. the parsed-meaning
     2. each of the entities in the context.
   The topic is the entity for which this value is maximized."
  (when (find-data agent 'applied-cxn)
    (let* ((entities-with-similarity
            (loop with applied-cxn = (find-data agent 'applied-cxn)
                  for entity in (entities (get-data agent 'context))
                  for sim = (concept-representations::concept-entity-similarity (meaning applied-cxn) entity)
                  collect (cons entity sim)))
           ;; if two entities have exactly the same
           ;; maximum similarity, interpretation fails
           (highest-pair (the-biggest #'cdr entities-with-similarity))
           (maybe-topic (first highest-pair))
           (candidates-count (count (rest highest-pair)
                                    entities-with-similarity
                                    :key #'cdr :test #'=)))
      (cond ((eq candidates-count 1)
             (set-data agent 'interpreted-topic maybe-topic))
            ((and (> candidates-count 1)
                  (zerop (sum (mapcar #'cdr entities-with-similarity))))
             ;; concept matches 0% with all entities
             (set-data agent 'interpreted-topic nil)
             (set-data agent 'interpreted-topic-reason 'no-match))
            (t
             ;; just fail
             (set-data agent 'interpreted-topic nil)
             (set-data agent 'interpreted-topic-reason 'more-candidates)))))
             
  ;; notify
  (notify event-interpretation-end agent)
  ;; returns nil or the value set
  (find-data agent 'interpreted-topic))
