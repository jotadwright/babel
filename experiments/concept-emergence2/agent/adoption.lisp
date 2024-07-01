(in-package :cle)

;; ----------------------------
;; + Adoption of construction +
;; ----------------------------

;;;; events
(define-event event-adopt-start (cxn cxn))

(defmethod adopt ((agent cle-agent) meaning form)
  "Adopt a new cxn into the lexicon of the agent."
  (let ((new-cxn (make-cxn agent meaning form)))
    ;; push the new construction
    (update-lexicon-inventory (lexicon agent) new-cxn)
    ;; update monitor
    (setf (invented-or-adopted agent) t)
    ;; notify
    (notify event-adopt-start new-cxn)
    ;; return created construction
    new-cxn))

(defmethod reset-adopt ((agent cle-agent) applied-cxn meaning)
  "Resets the cxn by resetting its meaning."
  (reset-cxn agent applied-cxn meaning)
  ;; if the cxn is in the trash, place it back in the lexicon
  (update-lexicon-inventory (lexicon agent) applied-cxn)
  ;; update monitor
  (setf (invented-or-adopted agent) t)
  ;; notify
  (notify event-adopt-start applied-cxn)
  ;; return created construction
  applied-cxn)
