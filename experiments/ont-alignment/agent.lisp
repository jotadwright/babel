(in-package :ont-alignment)

(defclass ont-alignment-agent (agent)
  ((personal-db
    :accessor personal-db
    :initform nil
    :documentation "The db which the agent is connected to")
   (personal-query-language
    :accessor query-lang
    :initform nil
    :documentation "The query language which is assigned to the agent to query its own database at the beggining of the experiment")
   (dictionary
    :accessor dictionary
    :initform nil
    :documentation "A set of the question-answer pairs that were already given")))

(defmethod make-ont-agents ((nb-of-agents string)(experiment experiment))
  "We create a population of agents"
  (let ((agents (loop for i from 1 to nb-of-agents
                      for agent-id = (read-from-string (format nil "agent-~d" i))
                      collect (make-instance 'ont-alignment-agent
                                             :id agent-id
                                             :query-lang "sql"
                                             :experiment experiment
                                             :personal-db "whatever"))))
    (setf (agents experiments) agents)))

;(random-n-elements 2 '(1 2 3 4 5))
