;;;; /world.lisp

(in-package :demo-wtnschp)

;; ------------------
;; + Sensory Object +
;; ------------------

(defclass sensory-object (entity)
  ((rgbcolor
    :type list :accessor rgbcolor :initform nil :initarg :rgbcolor
    :documentation "The color of the object in RGB")
   (xpos
    :type number :accessor xpos :initform 0 :initarg :xpos
    :documentation "The xpos of the object")
   (ypos
    :type number :accessor ypos :initform 0 :initarg :ypos
    :documentation "The ypos of the object"))
  (:documentation "A sensory object"))

(defun json->sensory-object (json &key id)
  "Generate a sensory object from a json object (returned by decode-json)"
  (make-instance 'sensory-object
                 :id (or id (make-id 'sensory-object))
                 :rgbcolor (rest (assoc :color json))
                 :xpos (rest (assoc :xpos json))
                 :ypos (rest (assoc :ypos json))))

(defmethod copy-object ((obj sensory-object))
  (make-instance 'sensory-object
                 :id (id obj)
                 :rgbcolor (copy-object (rgbcolor obj))
                 :xpos (xpos obj)
                 :ypos (ypos obj)))

(defmethod print-object ((obj sensory-object) stream)
  (format stream "<sensory-object: ~a>" (id obj)))

;; --------------
;; + Object Set +
;; --------------

(defclass object-set (entity)
  ((entities :accessor entities :initarg :entities :initform nil :type list
             :documentation "The entities of the set"))
  (:documentation "A set of sensory objects"))

(defun make-object-set (entities &key id)
  "Make an entity set from a list of entities"
  (declare (type list entities))
  (make-instance 'object-set
                 :entities entities
                 :id (or id (make-id 'entity-set))))

(defmethod equal-entity ((set-1 object-set) (set-2 object-set))
  "Two sets are equal when their components are equal"
  (permutation-of? (entities set-1) (entities set-2) :test #'equal-entity))

(defmethod find-entity-by-id ((set object-set) (id symbol))
  "Use ID to find an entity in a set"
  (if (eq (id set) id)
    set
    (find id (entities set) :key #'id)))

(defmethod all-entities-but-id ((set object-set) (id symbol))
  "Return all entities in the set, but the one with the given id"
  (remove id (entities set) :key #'id))

(defun json->object-set (json)
  "Create an object set from json data (returned by decode-json)"
  (let ((entities
         (loop for json-obj in json
               for object = (json->sensory-object (rest (first json-obj)) :id (caar json-obj))
               collect object)))
    (make-object-set entities)))

(defmethod copy-object ((set object-set))
  (make-instance 'object-set
                 :id (id set)
                 :entities (mapcar #'copy-object (entities set))))

(defmethod print-object ((set object-set) stream)
  (format stream "<object-set: ~a>" (id set)))