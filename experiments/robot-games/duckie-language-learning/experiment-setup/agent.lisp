(in-package :duckie-language-learning)

;; -----------------
;; + Agent Classes +
;; -----------------

(defclass duckie-language-learning-agent (agent)
  ((grammar :initarg :grammar :accessor grammar :initform (make-duckie-grammar-cxns)
            :type (or null fcg-construction-set)
            :documentation "The agent's grammar")
   (ontology :initarg :ontology :accessor ontology :initform *ontology*
             :type blackboard
             :documentation "The ontology of the agent"))
  (:documentation "Base class for duckie agent"))


(defmethod initialize-instance :after ((agent duckie-language-learning-agent) &key)
  (set-data (blackboard (grammar agent)) :owner agent))

(defmethod copy-object ((agent duckie-language-learning-agent))
  (make-instance 'duckie-language-learning-agent))

(defclass duckie-language-learning-simulation-agent (duckie-language-learning-agent)
  ((primitive-inventory :initarg :primitive-inventory :accessor primitive-inventory
                        :initform  *duckie-simulation-primitives* 
                        :type primitive-inventory
                        :documentation "The primitive inventory of the agent"))
  (:documentation "simulation agent with symbolic primitives"))


(defclass duckie-language-learning-world-agent (duckie-language-learning-agent)
  ((primitive-inventory :initarg :primitive-inventory :accessor primitive-inventory
                        :initform  *duckie-world-primitives* 
                        :type primitive-inventory
                        :documentation "The primitive inventory of the agent"))
  (:documentation "simulation agent with world primitives"))