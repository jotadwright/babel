(in-package :ont-alignment)

;we set a connection for each agent with its own db

(defclass ont-alignment-agent (agent)
  ((personal-db
    :accessor personal-db :initform nil
    :documentation "The db which the agent is connected to")
   (applied-cxn
    :accessor applied-cxn :initform nil
    :documentation "The applied cxn in the current interaction")
   (ontology
    :type blackboard :accessor ontology :initform nil)
    :documentation "The agent's ontology.")
   (grammar
    :type construction-set :accessor grammar :initform nil
    :documentation "The agent's grammar.")))

(defun make-ont-agents (nb-of-agents)
  "We create a population of agents"
  (let ((agents (loop for i from 1 to nb-of-agents
                      for agent-id = (read-from-string (format nil "agent-~d" i))
                      collect (make-instance 'ont-alignment-agent
                                             :id agent-id
                                             :experiment experiment
                                             :lexicon (make-agent-cxn-set)))))
    (setf (agents experiments) agents))
  )

