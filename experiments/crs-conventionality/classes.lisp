(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                              ;;
;; This file declares all classes proper to the crs-conventionality experiments ;;
;;                                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Experiments ;;
;;;;;;;;;;;;;;;;;

(defclass crs-conventionality-experiment (experiment)
  ()
  (:documentation "Experiment class."))

(defclass naming-game-experiment (crs-conventionality-experiment)
  ()
  (:documentation "Class for naming game experiment."))

(defmethod initialize-instance :after ((experiment naming-game-experiment) &key &allow-other-keys)
  "Creates the population of the experiment, each agent having an empty grammar and creates the world."
  ;; Set population
  (setf (population experiment)
        (loop for i from 1 to (get-configuration experiment :population-size)
              collect (make-instance 'naming-game-agent
                                     :id (intern (format nil "agent-~a" i))
                                     :grammar (initialize-grammar word-list)
                                     :experiment experiment)))
  ;; Set world
  (setf (world experiment)
        (make-instance 'syntax-world
                       :experiment experiment
                       :attributes (get-attributes-values-ontology ontology)))))


;; Agents ;;
;;;;;;;;;;;;

(defclass crs-conventionality-agent (agent)
  ((grammar 
    :documentation "The linguistic inventory of the agent."
    :type (or nil construction-inventory)
    :initform nil :initarg :grammar :accessor grammar))
  (:documentation "An agent in the experiment"))

(defclass naming-game-agent (crs-conventionality-agent)
  ()
  (:documentation "An agent in the experiment"))


;; Entities ;;
;;;;;;;;;;;;;;

(defclass crs-conventionality-entity (entity)
  ()
  (:documentation "An entity in the experiment"))


(defclass naming-game-entity (crs-conventionality-entity)
  ()
  (:documentation "An entity in the naming-game experiment"))


;; Worlds ;;
;;;;;;;;;;;;

(defclass crs-conventionality-world (entity)
  ((nr-of-entities
    :documentation "The number of entities in the world."
    :type number
    :initform 10 :accessor nr-of-entities))
  ((entities 
    :documentation "The entities themselves."
    :type list
    :initform nil :initarg :entities :accessor entities))
  (:documentation "A world in the experiment"))

(defclass naming-game-world (crs-conventionality-world)
  ()
  (:documentation "The naming game world."))

(defmethod initialize-instance ((world naming-game-world))
  "Initialises an instance of the naming game world."
  (loop for i from 1 upto (nr-of-entities world)
        collect (make-instance 'naming-game-entity
                               :id (intern (format nil "o-~a" i)))))

;; Entities ;;
;;;;;;;;;;;;;;

