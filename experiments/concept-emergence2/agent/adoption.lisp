(in-package :cle)

;; ------------------------
;; + Adoption of construction +
;; ------------------------

(defmethod adopt ((agent cle-agent) meaning form)
  (let ((new-cxn (make-cxn agent meaning form)))
    ;; push the new construction
    (push new-cxn (lexicon agent))
    ;; update monitor
    (setf (invented-or-adopted agent) t)
    ;; return created construction
    new-cxn))
