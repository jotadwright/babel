(in-package :roconaga)

;; ----------------
;; + Object Class +
;; ----------------

(defclass sensory-object (entity)
  ((rgb-color
    :documentation "rgb color of the object"
    :type list :accessor rgb-color
    :initarg :rgb-color)
   (lab-color
    :documentation "lab color of the object"
    :type list :accessor lab-color
    :initarg :lab-color)
   (x :accessor x-pos :initarg :x)
   (y :accessor y-pos :initarg :y))
  (:documentation "an object"))

;; --------------
;; + Object Set +
;; --------------

(defclass sensory-object-set (entity)
  ((entities
    :documentation "the objects in the set"
    :type list :accessor entities :initarg :entities :initform nil))
  (:documentation "a set of objects"))

(defun process-scene (observed-scene)
  (make-instance 'sensory-object-set
                 :id 'context
                 :entities (loop for object in observed-scene
                                 collect (make-instance 'sensory-object :id (caar object)
                                                        :rgb-color (rest (assoc :color (cdar object)))
                                                        :lab-color (rgb->lab (rest (assoc :color (cdar object))))
                                                        :x (rest (assoc :xpos (cdar object)))
                                                        :y (rest (assoc :ypos (cdar object)))))))

(defmethod equal-entity ((set1 sensory-object-set) (set2 sensory-object-set))
  (permutation-of? (entities set1) (entities set2) :test #'equal-entity))

(defmethod find-entity-by-id ((set sensory-object-set) id)
  (find id (entities set) :key #'id))

;; --------------------
;; + Color categories +
;; --------------------

(defclass color-category (entity)
  ((value
    :documentation "the lab colour values"
    :type list :initform nil :initarg :value :accessor value)
   (score
    :documentation "the score of the prototype"
    :type number :initform 0.5 :initarg :score :accessor score)
   (variance
    :documentation "the variance of the prototypial values"
    :type number :initform 0 :initarg :variance :accessor variance))
  (:documentation "a colour category"))

(defun make-color-category (object)
  (make-instance 'color-category
                 :id (make-id 'color-category)
                 :value (lab-color object)))

;; ---------------
;; + Agent Class +
;; ---------------

(defclass grounded-color-naming-game-agent (agent)
  ((context
    :documentation "the context of the current interaction"
    :type list :accessor context :initarg :context :initform nil :accessor world)
   (topic
    :documentation "the topic of the current interaction"
    :type (or null rcg-object) :accessor topic :initarg :topic :initform nil
    :accessor hypothesized-topic)
   (observed-object
    :documentation "object observed after pointing"
    :type (or null rcg-object) :accessor observed-object :initarg :observed-object :initform nil)
   (grammar
    :documentation "the lexicon of the agent"
    :type fcg-construction-set :accessor grammar :initarg :grammar)
   (ontology
    :documentation "the ontology of the agent"
    :type blackboard :accessor ontology :initarg :ontology)
   (applied-cxn
    :documentation "the cxn applied in this interaction"
    :type (or null fcg-construction) :accessor applied-cxn :initarg :applied-cxn :initform nil)
   (applied-category
    :documentation "the category applied in this interaction"
    :type (or null color-category) :accessor applied-category :initarg :applied-cat :initform nil)
   (irl-program
    :documentation "the irl program used in this interaction"
    :type list :accessor irl-program :initarg :irl-program :initform nil
    :accessor meaning-representation)
   (robot
    :documentation "the robot connection used by the agent"
    :type (or null nao) :accessor robot :initarg :robot :initform nil))
  (:documentation "agent"))

(defmethod speakerp ((agent grounded-color-naming-game-agent))
  (eql (discourse-role agent) 'speaker))

(defmethod hearerp ((agent grounded-color-naming-game-agent))
  (eql (discourse-role agent) 'hearer))

(defmethod initialize-instance :after ((agent grounded-color-naming-game-agent) &key)
  "Initialize the agent; make a cxn set and an ontology
   and introduce the available primitives"
  (setf (grammar agent) (make-agent-cxn-set)
        (ontology agent) (make-blackboard))
  (set-data (ontology agent) 'primitives
            '(get-context filter-by-closest-color))
  (set-data (ontology agent) 'color-categories nil))

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

(defmethod observed-utterance ((agent grounded-color-naming-game-agent))
  (utterance agent))

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

(define-configuration-default-value :robot-ips '("192.168.1.4"))
(define-configuration-default-value :robot-ports '("7850"))
(define-configuration-default-value :min-context-size 2)
(define-configuration-default-value :max-context-size 5)
(define-configuration-default-value :population-size 10)
(define-configuration-default-value :perceptual-deviation nil)
(define-configuration-default-value :silent nil)
(define-configuration-default-value :export-lexicon-interval 50)
(define-configuration-default-value :alignment-strategy :li)
(define-configuration-default-value :who-aligns :both)
(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0.1)
(define-configuration-default-value :alpha 0.1)

;; --------------------
;; + Experiment Class +
;; --------------------

(defclass grounded-color-naming-game-experiment (experiment)
  ((robots
    :documentation "A list of robot connections"
    :type list :initarg :robots :initform nil :accessor robots))
  (:documentation "Experiment class"))

(defmethod initialize-instance :after ((experiment grounded-color-naming-game-experiment) &key)
  "Initialize the experiment"
  ;; activate dot printing
  (activate-monitor print-a-dot-for-each-interaction)
  ;; activate disconnect-robots-after-series monitor
  (activate-monitor disconnect-robots-after-series)
  ;; set the population
  (setf (population experiment)
        (loop for i from 0 below (get-configuration experiment :population-size)
              collect (make-instance 'grounded-color-naming-game-agent :id i
                                     :experiment experiment)))
  ;; open the connection to the robots
  (setf (robots experiment)
        (loop for ip in (get-configuration experiment :robot-ips)
              for port in (get-configuration experiment :robot-ports)
              for robot = (make-robot :ip ip :server-port port
                                      :connect-automatically nil)
              do (make-new-connection robot :test-connection (not (get-configuration experiment :silent)))
              do (sleep 1)
              collect robot))
  ;; check if there are at least 2 robots, otherwise use the same one twice
  (when (length< (robots experiment) 2)
    (setf (robots experiment) (loop repeat 2 collect (first (robots experiment)))))
  ;; start up the scene server
  (start-scene-server))

(defmethod destroy ((experiment grounded-color-naming-game-experiment))
  "Some cleanup when manually running an experiment"
  ;; stop the scene server
  (stop-scene-server)
  ;; disconnect the robots
  (loop for robot in (robots experiment)
        do (disconnect-robot robot)))