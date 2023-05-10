(in-package :duckie-language-learning)

;; ---------------
;; + Interaction +
;; ---------------

(defmethod interact :before ((experiment duckie-language-learning-experiment)
                             interaction &key)
  (let ((agent (hearer interaction)))
    (set-data (blackboard (grammar agent)) :fcg-solution nil)
    (set-data (blackboard (grammar agent)) :answer-correct? nil)
    (set-data (blackboard (grammar agent)) :ontology (ontology agent))
    (set-data (blackboard (grammar agent)) :primitive-inventory (primitive-inventory agent))))

(defmethod interact ((experiment duckie-language-learning-experiment)
                     interaction &key)
  "Runs an interaction in which the user is prompted to ask a question."
  (let ((agent (hearer interaction))
        (utterance (downcase (capi:prompt-for-string "Enter your question:"))))
    ;; Step 1: prompt the question and set it
    (setf (utterance agent) utterance)
    ;; Step 2: comprehend using the cxn-inventory of the agent
    (multiple-value-bind (meaning cipn)
        (comprehend utterance :cxn-inventory (grammar agent))
      (run-alignment (hearer interaction) cipn)
      (add-past-scene (hearer interaction)))))

(defmethod interact :after ((experiment duckie-language-learning-experiment)
                     interaction &key))
