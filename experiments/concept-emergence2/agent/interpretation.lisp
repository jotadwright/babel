(in-package :cle)

;; ------------------
;; + Interpretation +
;; ------------------

;; events
(define-event event-interpretation-end (agent cle-agent))

(defmethod interpret ((agent cle-agent))
  "Computes the weighted similarity between
     1. the parsed-meaning
     2. each of the objects in the context.
   The topic is the object for which this value is maximized."
  (when (find-data agent 'applied-cxn)
    (let* ((objects-with-similarity
            (loop with applied-cxn = (find-data agent 'applied-cxn)
                  for object in (objects (get-data agent 'context))
                  for sim = (weighted-similarity agent object (meaning applied-cxn))
                  collect (cons object sim)))
           ;; if two objects have exactly the same
           ;; maximum similarity, interpretation fails
           (highest-pair (the-biggest #'cdr objects-with-similarity))
           (maybe-topic (first highest-pair))
           (candidates-count (count (rest highest-pair)
                                    objects-with-similarity
                                    :key #'cdr :test #'=)))
      (cond ((eq candidates-count 1)
             (set-data agent 'interpreted-topic maybe-topic))
            ((and (> candidates-count 1)
                  (zerop (sum (mapcar #'cdr objects-with-similarity))))
             ;; concept matches 0% with all objects
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
