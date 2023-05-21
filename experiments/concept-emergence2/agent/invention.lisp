(in-package :cle)

;; ------------------------
;; + Invention of concepts +
;; ------------------------

(defmethod invent ((agent cle-agent))
  (let* ((meaning (attributes (get-data agent 'topic)))
         (form (make-new-word))
         (new-cxn (make-cxn agent meaning form)))
    ;; push the new construction
    (push new-cxn (lexicon agent))
    ;; update monitor
    (setf (invented-or-adopted agent) t)
    ;; return created construction
    new-cxn))
