(in-package :cle)

;; ---------
;; + Agent +
;; ---------

(defclass cle-agent (agent social-network::social-network partner-selection::neighbour-q-values perceive usage-tracker)
  ((lexicon
    :documentation "The agent's lexicon."
    :type lexicon :accessor lexicon :initarg :lexicon :initform nil)
   (views
    :documentation "The views that the agent has over a world."
    :type list :accessor views :initarg :views :initform nil)
   (current-view
    :documentation "The current view assigned to the agent."
    :type string :accessor current-view :initform nil)
   (invented-or-adopted
    :documentation "Whether the agent invented or adopted during the interaction."
    :type boolean :accessor invented-or-adopted :initform nil)))

(defmethod clear-agent ((agent cle-agent))
  "Clear the slots of the agent for the next interaction."
  (setf (blackboard agent) nil
        (utterance agent) nil
        (invented-or-adopted agent) nil
        (perceived-objects agent) (make-hash-table)
        (communicated-successfully agent) nil))

(defmethod find-in-lexicon ((agent cle-agent) (form string))
  "Finds constructions with the given form in the lexicon of the given agent."
  (find-form-in-lexicon (lexicon agent) form))

(defmethod empty-lexicon-p ((agent cle-agent))
  (eq (lexicon-size (lexicon agent)) 0))

