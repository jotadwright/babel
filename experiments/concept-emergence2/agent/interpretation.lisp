(in-package :cle)

;; ------------------
;; + Interpretation +
;; ------------------

(define-event interpretation-finished (agent cle-agent))

(defmethod interpret ((agent cle-agent))
  "Computes the weighted similarity between
     1. the parsed-meaning
     2. each of the objects in the context.
   The topic is the object for which this value is maximized."
  (when (find-data agent 'applied-cxn)
    (let* ((objects-with-similarity
            (loop with applied-cxn = (find-data agent 'applied-cxn)
                  for object in (objects (get-data agent 'context))
                  for sim = (weighted-similarity object (meaning applied-cxn))
                  collect (cons object sim)))
           ;; if two objects have exactly the same
           ;; maximum similarity, interpretation fails
           (highest-pair (the-biggest #'cdr objects-with-similarity))
           (maybe-topic (first highest-pair))
           (similar-p (> (count (rest highest-pair)
                                  objects-with-similarity
                                  :key #'cdr :test #'=)
                           1)))
      (set-data agent 'interpreted-topic
                (unless similar-p maybe-topic))))
  (notify interpretation-finished agent) ;; notify
  (find-data agent 'interpreted-topic))

(defmethod reentrance (context cxn)
  "Interpret a given concept in the context."
  (let* ((objects-with-similarity
          (loop for object in (objects context)
                for sim = (weighted-similarity object (meaning cxn))
                collect (cons object sim)))
         ;; if two objects have exactly the same
         ;; maximum similarity, interpretation fails
         (highest-pair (the-biggest #'cdr objects-with-similarity))
         (maybe-topic (first highest-pair))
         (similar-p (> (count (rest highest-pair)
                                objects-with-similarity
                                :key #'cdr :test #'=)
                         1)))
    (if (not similar-p)
      maybe-topic
      nil)))
