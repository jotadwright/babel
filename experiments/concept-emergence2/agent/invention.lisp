(in-package :cle)

;; ------------------------
;; + Invention of concepts +
;; ------------------------

;; events
(define-event event-invention-end (cxn cxn))

(defmethod invent ((agent cle-agent))
  (let* ((meaning (get-data agent 'topic))
         (form (make-new-word))
         (new-cxn (make-cxn agent meaning form)))
    ;; push the new construction
    (push new-cxn (lexicon agent))
    ;; update monitor
    (setf (invented-or-adopted agent) t)
    ;; notify
    (notify event-invention-end new-cxn)
    ;; return created construction
    new-cxn))
