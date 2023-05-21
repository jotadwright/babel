(in-package :cle)

;; --------------
;; + Production +
;; --------------

(define-event production-finished (agent cle-agent))

(defmethod production (agent)
  "Produce as an utterance the form associated with the chosen concept."
  (let ((applied-cxn (get-data agent 'applied-cxn)))
    (setf (utterance agent) (downcase (mkstr (form applied-cxn))))
    (notify production-finished agent)))
