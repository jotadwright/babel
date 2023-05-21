(in-package :cle)

;; ------------------------
;; + Invention of concepts +
;; ------------------------

(define-event invention-finished (cxn cxn))

(defmethod invent (agent)
  (let* ((meaning (attributes (get-data agent 'topic)))
         (form (make-new-word))
         (new-cxn (make-cxn agent meaning form)))
    ;; push the new construction
    (push new-cxn (lexicon agent))
    ;; update monitor
    (setf (invented-or-adopted agent) t)
    ;; notify web-interface
    (notify invention-finished new-cxn)
    ;; return created construction
    new-cxn))
