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
   (applied-voc
    :documentation "The form used to describe the topic"
    :initarg :applied-voc
    :initform nil
    :accessor applied-voc)
   (pointed-object
    :accessor pointed-object
    :initarg :pointed-object
    :type symbol
    :initform nil)))

(defmethod make-agents ((experiment experiment))
  "Creates the different agents in the population of experiment"
  (let ((agents (loop for i from 1 to 10
                      for agent-id = (read-from-string (format nil "agent-~d" i))
                      collect (make-instance 'naming-game-agent
                                             :id agent-id
                                             :experiment experiment
                                             :lexicon (make-instance 'construction-inventory)))))
    (setf (agents experiment) agents)))

(defmethod produce ((agent naming-game-agent))
  "agent tries to produce a word that refers to the topic object"
  (let ((considered-voc '()) ;list of voc-items (form, meaning, score) that have meaning as the topic
        (chosen-voc nil)
        (lexicon (lexicon agent)))
    (when lexicon
      (loop for voc-item in lexicon
            do (if (eql (meaning voc-item) (topic agent))
                 (push voc-item considered-voc)))
      (cond ((= (length considered-voc) 1) (setf chosen-voc (first considered-voc)))
            ((> (length considered-voc) 1) (setf chosen-voc (highest-score-voc considered-voc)))))
    chosen-voc
    ))


