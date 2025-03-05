(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;;  Declares all classes proper to the crs-conventionality experiments  ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Experiments ;;
;;;;;;;;;;;;;;;;;

(defclass crs-conventionality-experiment (experiment)
  ()
  (:documentation "Experiment class."))


(defclass naming-game-experiment (crs-conventionality-experiment)
  ()
  (:documentation "Class for naming game experiment."))

(defclass concept-emergence-game-experiment (crs-conventionality-experiment)
  ()
  (:documentation "Class for concept emergence game experiment."))


(defmethod initialize-instance :after ((experiment naming-game-experiment) &key &allow-other-keys)
  "Creates the population and world of the experiment."
  ;; Set world
  (setf (world experiment)
        (make-instance 'naming-game-world :experiment experiment))
  ;; Set population
  (setf (population experiment)
        (make-instance 'naming-game-population :experiment experiment)))

(defmethod initialize-instance :after ((experiment concept-emergence-game-experiment) &key &allow-other-keys)
  "Creates the population and world of the experiment."
  ;; Set world
  (setf (world experiment)
        (make-instance 'concept-emergence-game-world :experiment experiment))
  ;; Set population
  (setf (population experiment)
        (make-instance 'concept-emergence-game-population :experiment experiment)))


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

(defclass concept-emergence-game-population (crs-conventionality-population)
  ()
  (:documentation "A population in the concept emergence game."))


(defmethod initialize-instance :after ((population naming-game-population) &key &allow-other-keys)
  "Creates the population of the experiment."
  ;; Set population
  (setf (agents population)
        (loop for i from 1 to (get-configuration (experiment population) :nr-of-agents-in-population)
              collect (make-instance 'naming-game-agent
                                     :id (intern (format nil "AGENT-~a" i))
                                     :experiment (experiment population)
                                     :population population))))

(defmethod initialize-instance :after ((population concept-emergence-game-population) &key &allow-other-keys)
  "Creates the population of the experiment."
  ;; Set population
  (setf (agents population)
        (loop for i from 1 to (get-configuration (experiment population) :nr-of-agents-in-population)
              collect (make-instance 'concept-emergence-game-agent
                                     :id (intern (format nil "AGENT-~a" i))
                                     :experiment (experiment population)
                                     :population population))))

(defclass crs-conventionality-agent (agent meta-layer-learning:object-w-learning)
  ((grammar 
    :documentation "The linguistic inventory of the agent."
    :type (or nil construction-inventory)
    :initform nil :initarg :grammar :accessor grammar)
   (population
    :documentation "A backpointer to the population to which the agent belongs."
    :type crs-conventionality-population
    :initform nil :initarg :population :accessor population)
   (conceptualised-utterance
    :documentation "The utterance that the agent as a hearer conceptualises."
    :type crs-conventionality-entity-set
    :initform nil :initarg :conceptualised-utterance :accessor conceptualised-utterance)
   (computed-topic
    :documentation "The topic that the agent computed as hearer."
    :type crs-conventionality-entity-set
    :initform nil :initarg :computed-topic :accessor computed-topic)
   (topic
    :documentation "The topic that the agent wants to formulate as speaker."
    :type crs-conventionality-entity-set
    :initform nil :initarg :topic :accessor topic)
   (applied-constructions
    :documentation "The topic that the agent wants to formulate as speaker."
    :type list
    :initform nil :initarg :applied-constructions :accessor applied-constructions)
   (solution-nodes
    :documentation ""
    :type list
    :initform nil :initarg :solution-nodes :accessor solution-nodes)
   (solution-nodes-conceptualisation
    :documentation ""
    :type list
    :initform nil :initarg :solution-nodes-conceptualisation :accessor solution-nodes-conceptualisation)
   (learning-rate
    :documentation "The learning rate of an agent. "
    :type list
    :initform nil :initarg :learning-rate :accessor learning-rate)
   (introduced-in-game
    :documentation "The game number in which the agent was introduced."
    :type number
    :initform 0 :initarg :introduced-in-game :accessor introduced-in-game))
  (:documentation "An agent in the experiment"))


(defmethod initialize-instance :after ((agent crs-conventionality-agent) &key &allow-other-keys)
  "Creates an agent of the population."
  (make-initial-grammar agent)
  (setf (learning-rate agent) (define-learning-rate agent (get-configuration (experiment (population agent)) :learning-strategy))))


(defmethod define-learning-rate (agent (mode (eql :default)))
  (get-configuration (experiment (population agent)) :learning-rate))


(defmethod print-object ((agent crs-conventionality-agent) stream)
  "Prints agent."
  (format stream "<agent: ~a>" (id agent)))


(defclass naming-game-agent (crs-conventionality-agent)
  ()
  (:documentation "An agent in the experiment"))

(defclass concept-emergence-game-agent (crs-conventionality-agent)
  ()
  (:documentation "An agent in the experiment"))


;; Worlds, scenes and Entities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ENTITIES
(defclass crs-conventionality-entity-set (entity)
  ((entities 
    :documentation "The entities themselves."
    :type list
    :initform nil :initarg :entities :accessor entities))
  (:documentation "Class for holding entities."))


(defmethod print-object ((entity-set crs-conventionality-entity-set) stream)
  "Prints entity-set."
  (format stream "<entity-set: ~{{~a~^, ~}}>" (mapcar #'id (entities entity-set))))


(defclass naming-game-entity-set (crs-conventionality-entity-set)
  ()
  (:documentation "Class for holding naming game entities."))


(defclass concept-emergence-game-entity-set (crs-conventionality-entity-set)
  ()
  (:documentation "Class for holding concept emergence game entities."))

;; WORLD
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

(defclass concept-emergence-game-world (crs-conventionality-world)
  ()
  (:documentation "The concept emergence game world."))

(defmethod initialize-instance :after ((world naming-game-world) &key &allow-other-keys)
  "Sets the entities of the world."
  (setf (entities world)
        (loop for i from 1 to (get-configuration (experiment world) :nr-of-entities-in-world)
              collect (make-instance 'naming-game-entity
                                     :id (intern (format nil "OBJECT-~a" i))
                                     :world world))))

(defmethod initialize-instance :after ((world concept-emergence-game-world) &key &allow-other-keys)
  "Sets the entities of the world."
  (let ((entities (concept-representations::load-dataset (create-path (get-configuration (experiment world) :dataset)
                                                                      (get-configuration (experiment world) :datasplit)))))
    (setf (entities world)
          (random-elts entities (get-configuration (experiment world) :nr-of-entities-in-world)))))

;; SCENE       
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


(defclass concept-emergence-game-scene (crs-conventionality-scene)
  ()
  (:documentation "A concept-emergence game scene."))

;; ENTITY
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
  (:documentation "An entity in the naming game experiment"))


(defclass concept-emergence-game-entity (crs-conventionality-entity)
  ()
  (:documentation "An entity in the concept emergence game experiment"))


;; Interaction ;;
;;;;;;;;;;;;;;;;;

(defclass crs-conventionality-interaction (interaction)
  ((scene 
    :documentation "The scene in which the interaction takes place."
    :initform nil :initarg :scene :accessor scene)
   (topic 
    :documentation "The topic of the interaction."
    :initform nil :initarg :topic :accessor topic)
   (coherence
    :documentation "Whether both interacting agents would have said the same thing under the same circumstances."
    :initform nil :accessor coherence))
  (:documentation "An interaction in the experiment"))
