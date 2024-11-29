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
  "Creates the population and world of the experiment."
  ;; Set population
  (setf (population experiment)
        (make-instance 'naming-game-population :experiment experiment))
  ;; Set world
  (setf (world experiment)
        (make-instance 'naming-game-world :experiment experiment)))
  
                     
;; Populations and Agents ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crs-conventionality-population (entity)
  ((agents 
    :documentation "The agents in the population."
    :type list
    :initform nil :initarg :agents :accessor agents)
   (experiment
    :documentation "A backpointer to the experiment."
    :type crs-conventionality-experiment
    :initform nil :initarg :experiment :accessor experiment))
  (:documentation "A population in the experiment."))

(defmethod print-object ((population crs-conventionality-population) stream)
  "Prints population."
  (format stream "<population: ~a agents>" (length (agents population))))


(defclass naming-game-population (crs-conventionality-population)
  ()
  (:documentation "A population in the naming game."))

(defmethod initialize-instance :after ((population naming-game-population) &key &allow-other-keys)
  "Creates the population of the experiment."
  ;; Set population
  (setf (agents population)
        (loop for i from 1 to (get-configuration (experiment population) :nr-of-agents-in-population)
              collect (make-instance 'naming-game-agent
                                     :id (intern (format nil "AGENT-~a" i))
                                     :population population))))


(defclass crs-conventionality-agent (agent)
  ((grammar 
    :documentation "The linguistic inventory of the agent."
    :type (or nil construction-inventory)
    :initform nil :initarg :grammar :accessor grammar)
   (population
    :documentation "A backpointer to the population to which the agent belongs."
    :type crs-conventionality-population
    :initform nil :initarg :population :accessor population))
  (:documentation "An agent in the experiment"))

(defmethod print-object ((agent crs-conventionality-agent) stream)
  "Prints agent."
  (format stream "<agent: ~a>" (id agent)))


(defclass naming-game-agent (crs-conventionality-agent)
  ()
  (:documentation "An agent in the experiment"))

(defmethod initialize-instance :after ((agent naming-game-agent) &key &allow-other-keys)
  "Creates an agent of the population."
  (setf (grammar agent) (make-initial-grammar agent)))


;; Worlds, scenes and Entities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass crs-conventionality-entity-set (entity)
  ((entities 
    :documentation "The entities themselves."
    :type list
    :initform nil :initarg :entities :accessor entities))
  (:documentation "(Abstract) class for holding entities."))


(defclass crs-conventionality-world (crs-conventionality-entity-set)
  ((experiment
    :documentation "A backpointer to the experiment."
    :type crs-conventionality-experiment
    :initform nil :initarg :experiment :accessor experiment))
  (:documentation "A world in the experiment"))

(defmethod print-object ((world crs-conventionality-world) stream)
  "Prints world."
  (format stream "<world: ~a entities>" (length (entities world))))


(defclass naming-game-world (crs-conventionality-world)
  ()
  (:documentation "The naming game world."))


(defmethod initialize-instance :after ((world naming-game-world) &key &allow-other-keys)
  "Sets the entities of the world."
  (setf (entities world)
        (loop for i from 1 to (get-configuration (experiment world) :nr-of-entities-in-world)
              collect (make-instance 'naming-game-entity
                                     :id (intern (format nil "OBJECT-~a" i))
                                     :world world))))


(defclass crs-conventionality-scene (crs-conventionality-entity-set)
  ((interaction
    :documentation "A backpointer to the interaction to which the scene belongs."
    :type crs-conventionality-interaction
    :initform nil :initarg :interaction :accessor interaction))
  (:documentation "A scene in an interaction"))

(defmethod print-object ((scene crs-conventionality-scene) stream)
  "Prints scene."
  (format stream "<scene: ~a entities>" (length (entities scene))))


(defclass naming-game-scene (crs-conventionality-scene)
  ()
  (:documentation "A naming game scene."))


(defclass crs-conventionality-entity (entity)
  ((world
    :documentation "A backpointer to the world to which the entity belongs."
    :type crs-conventionality-world
    :initform nil :initarg :world :accessor world))
  (:documentation "An entity in the experiment"))

(defmethod print-object ((entity crs-conventionality-entity) stream)
  "Prints entity."
  (format stream "<entity: ~a>" (id entity)))


(defclass naming-game-entity (crs-conventionality-entity)
  ()
  (:documentation "An entity in the naming-game experiment"))


;; Interaction ;;
;;;;;;;;;;;;;;;;;

(defclass crs-conventionality-interaction (interaction)
  ((scene 
    :documentation "The scene in which the interaction takes place."
    :initform nil :initarg :scene :accessor scene)
   (topic 
    :documentation "The topic of the interaction."
    :initform nil :initarg :topic :accessor topic))
  (:documentation "An interaction in the experiment"))