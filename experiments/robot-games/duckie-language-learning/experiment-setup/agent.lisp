(in-package :duckie-language-learning)

;; -----------------
;; + Agent Classes +
;; -----------------

(defclass duckie-language-learning-agent (agent)
  ((grammar :initarg :grammar :accessor grammar :initform (make-duckie-grammar-cxns)
            :type (or null fcg-construction-set)
            :documentation "The agent's grammar")
   (ontology :initarg :ontology :accessor ontology :initform *ontology* ;; GLOBAL VARIABLE!!
             :type blackboard
             :documentation "The ontology of the agent")
   (memory :initarg :memory :accessor memory :initform (make-hash-table :test #'eq)
           :documentation "The agent's memory (used by the composer)"))
  (:documentation "Base class for duckie agent"))

(defmethod initialize-instance :after ((agent duckie-language-learning-agent) &key)
  (set-data (blackboard (grammar agent)) :owner agent))

(defmethod copy-object ((agent duckie-language-learning-agent))
  (make-instance 'duckie-language-learning-agent))

;; --------------------
;; + Simulation Agent +
;; --------------------
(defclass duckie-language-learning-simulation-agent (duckie-language-learning-agent)
  ((primitive-inventory :initarg :primitive-inventory :accessor primitive-inventory
                        :initform  *duckie-simulation-primitives* 
                        :type primitive-inventory
                        :documentation "The primitive inventory of the agent"))
  (:documentation "simulation agent with symbolic primitives"))

;; -------------------
;; + Real-life Agent +
;; -------------------
(defclass duckie-language-learning-world-agent (duckie-language-learning-agent)
  ((primitive-inventory :initarg :primitive-inventory :accessor primitive-inventory
                        :initform  *duckie-world-primitives* 
                        :type primitive-inventory
                        :documentation "The primitive inventory of the agent"))
  (:documentation "simulation agent with world primitives"))

;; -------------------
;; + Memory strategy +
;; -------------------
(defun add-past-scene (agent)
  "Store all past scenes with the applied utterance and correct answer
     and use these when composing a new program (the new program must
     lead to the correct answer in all of these scenes)."
  (let* ((world (find-data (ontology agent) 'world))
         (sample (cons world (find-data (blackboard (grammar agent)) :ground-truth-topic)))
         (hash-key (sxhash (utterance agent)))
         (window-size (get-configuration agent :composer-past-scenes-window)))
    (if (gethash hash-key (memory agent))
      (if (>= (length (gethash hash-key (memory agent))) window-size)
        (setf (gethash hash-key (memory agent))
              (cons sample (butlast (gethash hash-key (memory agent)))))
        (push sample (gethash hash-key (memory agent))))
      (setf (gethash hash-key (memory agent)) (list sample)))))
