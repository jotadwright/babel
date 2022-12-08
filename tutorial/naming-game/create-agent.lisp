(in-package :cl-user)

(defmethod make-world ((experiment experiment))
  "Creates the different objects in the world of experiment"
  (let ((objects (loop for i from 1 to 10
                       collect (read-from-string (format nil "obj-~d" i)))))
    (setf (world experiment) objects)))

(defmethod make-agents ((experiment experiment))
  "Creates the different agents in the population of experiment"
  (let ((agents (loop for i from 1 to 10
                      for agent-id = (read-from-string (format nil "agent-~d" i))
                      collect (make-instance 'agent
                                             :id agent-id
                                             :experiment experiment
                                             :configurations (configurations experiment)))))
    (setf (agents experiment) agents)))

(defclass naming-game-agent (agent)
  ((lexicon
    :documentation "The lexicon of the agent"
    :initarg :lexicon
    :accessor lexicon
    :initform nil
    :type list)
   (topic
    :documentation "The object the agent interacts about"
    :initarg :topic
    :accessor topic
    :initform nil
    :type symbol)
   (applied-voc
    :documentation "The form used to describe the topic"
    :initarg :applied-voc
    :initform nil
    :accessor applied-voc)))
