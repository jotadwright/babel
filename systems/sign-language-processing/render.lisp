(in-package :slp)

(defmethod render ((ts coupled-feature-structure) (mode (eql :set-of-predicates)) &key &allow-other-keys)
  "extracts all form features from the resulting transient structure in production and represents them as a set of predicates"
  (extract-forms (left-pole-structure ts)))

(define-event-handler (trace-fcg produce-finished)
  (let* ((visualisation (represent-signs utterance)))
    (add-element `((h3) ,(format nil "Produced the following signed utterance:")))
    (add-element visualisation)))

(define-event-handler (trace-fcg-processing-level produce-all-finished)
  (let* ((visualisation (represent-signs utterance)))
    (add-element `((h3) ,(format nil "Produced the following signed utterances:")))
    (add-element visualisation)))
