;;;; clevr-world.lisp

(in-package :clevr)

;; ----------------------------------------------------------------;;
;; This file contains class definitions for objects and properties ;;
;; of the CLEVR world. Additionally, it contains functions to read ;;
;; the world from a file                                           ;;
;; ----------------------------------------------------------------;;

;; CATEGORIES
(defclass category (entity) ()
  (:documentation "Abstract base class for all categories"))

(defclass shape-category (category)
  ((shape :type symbol :initarg :shape :reader shape))
  (:documentation "A shape category"))

(defclass size-category (category)
  ((size :type symbol :initarg :size :reader size))
  (:documentation "A size category"))

(defclass color-category (category)
  ((color :type symbol :initarg :color :reader color))
  (:documentation "A color category"))

(defclass material-category (category)
  ((material :type symbol :initarg :material :reader material))
  (:documentation "A material category"))

(defclass spatial-relation-category (category)
  ((spatial-relation :type symbol :initarg :spatial-relation :reader spatial-relation))
  (:documentation "A spatial relation category"))

(defclass boolean-category (category)
  ((bool :type bool :initarg :bool :reader bool))
  (:documentation "An entity representation of booleans"))

(defclass attribute-category (category)
  ((attribute :type symbol :initarg :attribute :reader attribute))
  (:documentation "A category to represent object attributes"))

;; ONTOLOGY
(defgeneric add-category-to-ontology (ontology value category)
  (:documentation "Add a value of the given category to the ontology"))

(defmethod add-category-to-ontology (ontology value (category (eql 'shape)))
  (push-data ontology 'shapes (make-instance 'shape-category :id value :shape value)))
(defmethod add-category-to-ontology (ontology value (category (eql 'size)))
  (push-data ontology 'sizes (make-instance 'size-category :id value :size value)))
(defmethod add-category-to-ontology (ontology value (category (eql 'color)))
  (push-data ontology 'colors (make-instance 'color-category :id value :color value)))
(defmethod add-category-to-ontology (ontology value (category (eql 'material)))
  (push-data ontology 'materials (make-instance 'material-category :id value :material value)))
(defmethod add-category-to-ontology (ontology value (category (eql 'relation)))
  (push-data ontology 'spatial-relations (make-instance 'spatial-relation-category :id value :spatial-relation value)))
(defmethod add-category-to-ontology (ontology value (category (eql 'attribute)))
  (push-data ontology 'attributes (make-instance 'attribute-category :id value :attribute value)))

(defun build-clevr-ontology ()
  "Build the clevr ontology from the metadata file.
   How to handle 'thing'? Also consider this a shape?"
  (let* ((input-file (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-gen" "question_generation")
                                                     :name "metadata" :type "json")
                                      cl-user:*babel-corpora*))
         (metadata (decode-json-from-source input-file))
         (metadata-types (cdr (assoc :types metadata)))
         (clevr-ontology (make-blackboard)))
    (loop for (category-key . vocab) in metadata-types
          for category = (internal-symb (upcase (subseq (mkstr category-key) 1)))
          when vocab
          do (loop for value-str in vocab
                   for value = (internal-symb (upcase value-str))
                   do (add-category-to-ontology clevr-ontology value category)))
    ;; manually add 'thing' as a shape
    (add-category-to-ontology clevr-ontology 'thing 'shape)
    ;; manually add true and false to the ontology
    (push-data clevr-ontology 'booleans (make-instance 'boolean-category :id 'yes :bool t))
    (push-data clevr-ontology 'booleans (make-instance 'boolean-category :id 'no :bool nil))
    ;; manually add all attributes to the ontology
    (loop for attribute in '(shape size material color)
          do (add-category-to-ontology clevr-ontology attribute 'attribute))
    clevr-ontology))

;; GLOBAL VARIABLE *CLEVR-ONTOLOGY*
(setf *clevr-ontology* (build-clevr-ontology))

;; OBJECTS
(defclass clevr-object (entity)
  ((shape :type symbol :initarg :shape :accessor shape)
   (size :type symbol :initarg :size :accessor size)
   (color :type symbol :initarg :color :accessor color)
   (material :type symbol :initarg :material :accessor material)
   (relationships :type list :initarg :relationships :accessor relationships))
  (:documentation "An object in the CLEVR world"))

(defclass clevr-object-set (entity)
  ((objects :type list :initarg :objects :accessor objects :initform nil))
  (:documentation "A set of objects in the CLEVR world"))

(defmethod initialize-instance :around ((set clevr-object-set) &rest initargs &key id)
  (apply #'call-next-method set :id (or id (make-id 'set)) initargs))

(defmethod equal-entity ((set1 clevr-object-set)
                         (set2 clevr-object-set))
  "Two sets are equal if they are a permutation of each other.
   #'equal-entity compares the IDs of the objects"
  (permutation-of? (objects set1) (objects set2) :test #'equal-entity))

(defmethod find-entity-by-id ((set clevr-object-set) (id symbol))
  (find id (objects set) :key #'id))

;; COPY OBJECT
(defmethod copy-object ((shape-cat shape-category))
  (make-instance 'shape-category
                 :id (id shape-cat)
                 :shape (shape shape-cat)))

(defmethod copy-object ((size-cat size-category))
  (make-instance 'size-category
                 :id (id size-cat)
                 :size (size size-cat)))

(defmethod copy-object ((color-cat color-category))
  (make-instance 'color-category
                 :id (id color-cat)
                 :color (color color-cat)))

(defmethod copy-object ((material-cat material-category))
  (make-instance 'material-category
                 :id (id material-cat)
                 :material (material material-cat)))

(defmethod copy-object ((spatial-cat spatial-relation-category))
  (make-instance 'spatial-relation-category
                 :id (id spatial-cat)
                 :spatial-relation (spatial-relation spatial-cat)))

(defmethod copy-object ((bool-cat boolean-category))
  (make-instance 'boolean-category
                 :id (id bool-cat)
                 :bool (bool bool-cat)))

(defmethod copy-object ((attribute-cat attribute-category))
  (make-instance 'attribute-category
                 :id (id attribute-cat)
                 :attribute (attribute attribute-cat)))

(defmethod copy-object ((obj clevr-object))
  (make-instance 'clevr-object
                 :id (id obj)
                 :shape (shape obj)
                 :size (size obj)
                 :color (color obj)
                 :material (material obj)
                 :relationships (copy-object (relationships obj))))

(defmethod copy-object ((obj-set clevr-object-set))
  (make-instance 'clevr-object-set
                 :id (id obj-set)
                 :objects (mapcar #'copy-object (objects obj-set))))