(in-package :cle)

;; --------------
;; + Production +
;; --------------

;; events
(define-event event-production-end (agent cle-agent))

(defmethod production (agent)
  "Produce as an utterance the form associated with the chosen concept."
  (let ((applied-cxn (get-data agent 'applied-cxn)))
    (setf (utterance agent) (downcase (mkstr (form applied-cxn))))
    (notify event-production-end agent)))
