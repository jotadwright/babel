(in-package :cle)

;; ---------
;; + Agent +
;; ---------

(defclass cle-agent (agent)
  ((lexicon
    :documentation "The agent's lexicon."
    :type list :accessor lexicon :initform nil)
   (invented-or-adopted
    :documentation "Whether the agent invented or adopted during the interaction."
    :type boolean :accessor invented-or-adopted :initform nil)))

(defmethod clear-agent ((agent cle-agent))
  "Clear the slots of the agent for the next interaction."
  (setf (blackboard agent) nil
        (utterance agent) nil
        (invented-or-adopted agent) nil
        (communicated-successfully agent) nil))

(defmethod find-in-lexicon ((agent cle-agent) (form string))
  "Finds constructions with the given form in the lexicon of the given agent."
  (find form (lexicon agent) :key #'form :test #'string=))