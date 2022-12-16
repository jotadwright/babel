(in-package :naming-game)

(defclass naming-game-agent (agent)
  ((lexicon
    :documentation "The lexicon of the agent"
    :initarg :lexicon
    :accessor lexicon
    :initform nil
    :type construction-inventory)
   (topic
    :documentation "The object the agent interacts about"
    :initarg :topic
    :accessor topic
    :initform nil
    :type symbol)
   (applied-cxn
    :documentation "The form used to describe the topic"
    :initarg :applied-cxn
    :initform nil
    :accessor applied-cxn)
   (pointed-object
    :accessor pointed-object
    :initarg :pointed-object
    :type symbol
    :initform nil)))

(defun make-agent-cxn-set ()
  (let ((grammar-name (make-const "agent-grammar")))
    (eval
     `(def-fcg-constructions ,grammar-name
                             :cxn-inventory ,grammar-name
                             :feature-types ((args sequence)
                                             (form set-of-predicates)
                                             (meaning set-of-predicates)
                                             (subunits set)
                                             (footprints set))))))

(defmethod make-agents ((experiment experiment))
  "Creates the different agents in the population of experiment"
  (let ((agents (loop for i from 1 to 10
                      for agent-id = (read-from-string (format nil "agent-~d" i))
                      collect (make-instance 'naming-game-agent
                                             :id agent-id
                                             :experiment experiment
                                             :lexicon (make-agent-cxn-set)))))
    (setf (agents experiment) agents)))

(defmethod naming-game-produce ((agent naming-game-agent))
  "agent tries to produce a word that refers to the topic object"
  (multiple-value-bind (utterance solution cip)
      (produce (list (topic agent)) (lexicon agent))
    (values utterance (cxn-applied (top-node cip)))))
