(in-package :origins-of-syntax)

;;;;;;;;;;;;;;;;;;;;;;;
;; Class definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defclass syntax-experiment (experiment)
  ()
  (:documentation "Experiment class for origins of syntax experiment."))

(defclass syntax-grammar (fcg-construction-set)
  ()
  (:documentation "class for syntax grammar"))

(defclass syntax-agent (agent object-w-learning)
  ((grammar 
    :documentation "The construction inventory of the agent."
    :type (or nil syntax-grammar)
    :initform nil :initarg :grammar :accessor grammar))
  (:documentation "Syntax agents have a syntax grammar."))

(defclass set-of-syntax-objects (entity)
  ((objects
    :documentation "A list of all syntax-objects in the world"
    :type list :initform nil :accessor objects :initarg :objects))
  (:documentation "Inherit from me for being visualised as a set of syntax objects."))

(defmethod copy-object-content ((source set-of-syntax-objects) (destination set-of-syntax-objects))
  "Copies a syntax-object."
  (setf (objects destination) (objects source)))

(defclass syntax-world (set-of-syntax-objects) 
  ((experiment
    :documentation "A pointer to the experiment"
    :initform nil :accessor experiment :initarg :experiment)
   (attributes
    :documentation "Attributes that exist in the wolrd, and their values."
    :initform nil :type symbol :accessor attributes :initarg :attributes))
  (:documentation "A world consisting of a number of objects"))

(defclass syntax-scene (set-of-syntax-objects)
  ((experiment
    :documentation "A pointer to the experiment"
    :initform nil :accessor experiment :initarg :experiment))
  (:documentation "Class for scene."))

(defmethod copy-object-content ((source syntax-scene) (destination syntax-scene))
  (setf (experiment destination) (experiment source)))

(defclass syntax-topic (set-of-syntax-objects)
  ()
  (:documentation "Class for topic."))

(defclass syntax-object (entity) 
  ((id
    :documentation "ID of the object."
    :initform nil :type integer :accessor id :initarg :id)   
   (attributes
    :documentation "Alist of attributes of the object."
    :initform nil :type list :accessor attributes :initarg :attributes))
  (:documentation "An object in the syntax-world"))

(defmethod copy-object-content ((source syntax-object) (destination syntax-object))
  "Copies a syntax-object."
  (setf destination (make-instance 'syntax-object
                                    :id (id source)
                                    :attributes (attributes source))))

(defmethod copy-object-content ((source entity) (destination entity))
  "Does not copy!"
  (setf destination source))

(defgeneric competing-cxns (cxn cxn-inventory mode)
  (:documentation "Returns competing constructions of cxn in cxn-inventory according to mode."))

(defmethod competing-cxns ((cxn t) (cxn-inventory t) (mode t))
  (error (format nil "Please implement a competing-cxns method for cxn: ~a, cxn-inventory: ~a and mode: ~a"
                 (type-of cxn) (type-of cxn-inventory) mode)))

(defgeneric set-diagnostics-and-repairs (agent mode)
  (:documentation "Sets the diagnostics and repairs of agent according to mode."))

(defmethod set-diagnostics-and-repairs ((agent t) (mode t))
  (error (format nil "Please implement a set-diagnostics-and-repairs method for agent-class: ~a and mode: ~a"
                 (type-of agent) mode)))

(defgeneric initialize-lexicon (word-list mode)
  (:documentation "Initializes the lexicon according to mode."))

(defmethod initialize-lexicon ((word-list t) (mode t))
  (error (format nil "Please implement an initialize-lexicon method for mode: ~a" mode)))

(export '(group))