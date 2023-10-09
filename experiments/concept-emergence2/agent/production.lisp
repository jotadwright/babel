(in-package :cle)

;; --------------
;; + Production +
;; --------------

;; events
(define-event event-production-end (agent cle-agent))

(defmethod production (agent)
  "Produce the form associated with found cxn."
  (let ((applied-cxn (get-data agent 'applied-cxn)))
    (setf (utterance agent) (downcase (mkstr (form applied-cxn))))
    (update-usage-count agent (form applied-cxn))
    ;(update-history agent applied-cxn)
    (notify event-production-end agent)))
