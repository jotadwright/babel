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
    (loop with best-candidate = nil
          with applied-cxn = (find-data agent 'applied-cxn)
          for object in (objects (get-data agent 'context))
          if (eq (get-topic-id object) (obj-id (meaning applied-cxn)))
            do (set-data agent 'interpreted-topic object)))
             
  ;; notify
  (notify event-interpretation-end agent)
  ;; returns nil or the value set
  (find-data agent 'interpreted-topic))
