(in-package :cle)

;; ------------------------
;; + Adoption of construction +
;; ------------------------

;;;; events
(define-event new-cxn-added (cxn cxn))

(defmethod adopt ((agent cle-agent) meaning form)
  (let ((new-cxn (make-cxn agent meaning form)))
    ;; push the new construction
    (push new-cxn (lexicon agent))
    ;; update monitor
    (setf (invented-or-adopted agent) t)
    ;; notify web-interface
    (notify new-cxn-added new-cxn)
    ;; return created construction
    new-cxn))
