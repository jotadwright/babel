(in-package :cle)

;; ----------------------------
;; + Adoption of construction +
;; ----------------------------

;;;; events
(define-event event-adopt-start (cxn cxn))

(defmethod adopt ((agent cle-agent) meaning form)
  (let ((new-cxn (make-cxn agent meaning form)))
    ;; push the new construction
    (push new-cxn (lexicon agent))
    ;; update monitor
    (setf (invented-or-adopted agent) t)
    ;; notify
    (notify event-adopt-start new-cxn)
    ;; return created construction
    new-cxn))
