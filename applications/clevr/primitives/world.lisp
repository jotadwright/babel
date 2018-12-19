;;;; clevr-world.lisp

(in-package :clevr)

;; ----------------------------------------------------------------;;
;; This file contains class definitions for objects, properties    ;;
;; and questions of the CLEVR world. It also contains functions    ;;
;; for reading and parsing these from files.                       ;;
;; ----------------------------------------------------------------;;

;; CATEGORIES
(defclass category (entity) ()
  (:documentation "Abstract base class for all categories"))

(defclass attribute (category) ()
  (:documentation "Abtract base class for object attributes"))

(defclass shape-category (attribute)
  ((shape :type symbol :initarg :shape :reader shape))
  (:documentation "A shape category"))

(defclass size-category (attribute)
  ((size :type symbol :initarg :size :reader size))
  (:documentation "A size category"))

(defclass color-category (attribute)
  ((color :type symbol :initarg :color :reader color))
  (:documentation "A color category"))

(defclass material-category (attribute)
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
  (let* ((metadata-file (babel-pathname :directory '("applications" "clevr" "data")
                                        :name "metadata" :type "json"))
         (metadata (decode-json-from-source metadata-file))
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

;; OBJECT SET
(defclass clevr-object-set (entity)
  ((objects :type list :initarg :objects :accessor objects :initform nil)
   (image-index :type (or null number) :initarg :image-index :accessor image-index :initform nil)
   (image-filename :type (or null string) :initarg :image-filename :accessor image-filename :initform nil))
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

;; READING OBJECTS FROM FILE
(defun json-key->symbol (json-object key)
  (internal-symb (upcase (mkstr (rest (assoc key json-object))))))

(defun make-clevr-object (json-object relationships &key id)
  "Process a single object from JSON"
  (make-instance 'clevr-object
                 :id (or id (make-id 'obj))
                 :shape (json-key->symbol json-object :shape)
                 :size (json-key->symbol json-object :size)
                 :color (json-key->symbol json-object :color)
                 :material (json-key->symbol json-object :material)
                 :relationships relationships))

(defun collect-relations-for-object (list-of-json-relationships index)
  "Collect the spatial relationships for a single object"
  (loop for (relationship-key . list-of-lists) in list-of-json-relationships
        for relationship = (internal-symb (upcase (mkstr relationship-key)))
        collect (cons relationship (nth index list-of-lists))))

(defun process-object (json-object id-relations id-map id-mappings)
  (let* ((relationships (loop for (relation . list-of-ids) in id-relations
                              collect (cons relation
                                            (loop for id in list-of-ids
                                                  collect (rest (assoc id id-mappings)))))))
    (make-clevr-object json-object relationships :id (cdr id-map))))

(defun process-json-context (json-context)
  "Process the json context into a clevr-object-set object."
  (let* ((list-of-json-objects (rest (assoc :objects json-context)))
         (list-of-json-relationships (rest (assoc :relationships json-context)))
         (id-mappings (loop for i from 0 below (length list-of-json-objects)
                            collect (cons i (make-id 'obj)))))
    (make-instance 'clevr-object-set
                   :id 'clevr-context
                   :image-index (rest (assoc :image--index json-context))
                   :image-filename (rest (assoc :image--filename json-context))
                   :objects (loop for json-object in list-of-json-objects
                                  for id-map in id-mappings
                                  for id-relations = (collect-relations-for-object
                                                      list-of-json-relationships
                                                      (first id-map))
                                  collect (process-object json-object
                                                          id-relations
                                                          id-map
                                                          id-mappings)))))

(defun read-contexts-from-file (contexts-file)
  "Read the clevr scenes from a file. This functions expects a file
   where each line contains a json object of a context. If this is
   not the case, you can use json->lines in json-utils.lisp.
   Returns a list of 'clevr-object-set objects."
  (let ((contexts 
         (with-open-file (stream contexts-file :direction :input)
           (mapcar #'decode-json-from-string
                   (stream->list stream)))))
    (mapcar #'process-json-context contexts)))

;; QUESTION
(defclass clevr-question (entity)
  ((question-index
    :documentation "An index for the question"
    :type number :initarg :question-index :initform 0 :accessor question-index)
   (question
    :documentation "The actual question string"
    :type string :initarg :question :initform "" :accessor question)
   (image-index
    :documentation "The image index this question belongs to"
    :type (or null number) :initarg :image-index :accessor image-index :initform nil)
   (image-filename
    :documentation "The image filename this question belongs to"
    :type (or nill string) :initarg :image-filename :accessor image-filename :initform nil)
   (answer
    :documentation "The correct answer to the question"
    :initarg :answer :initform nil :accessor answer)
   (program
    :documentation "The alist notation of the functional program"
    :initarg :program :initform nil :type list :accessor program)
   (dataset-split
    :documentation "The dataset split this question belongs to"
    :initarg :dataset-split :initform nil :type (or null string) :accessor dataset-split)
   (template-filename
    :documentation "The filename of the question template"
    :initarg :template-filename :initform nil :type (or null string) :accessor template-filename)
   (question-family-index
    :documentation "The index of the question family; depends on template file"
    :initarg :question-family-index :initform 0 :type number :accessor question-family-index))
  (:documentation "A question from the clevr dataset"))

(defmethod initialize-instance :around ((q clevr-question) &rest initargs &key id)
  (apply #'call-next-method q :id (or id (make-id 'question)) initargs))

;; READ QUESTIONS FROM FILE
(defun process-json-question (json-question)
  (make-instance 'clevr-question
                 :question-index (rest (assoc :question--index json-question))
                 :question (downcase (rest (assoc :question json-question)))
                 :image-index (rest (assoc :image--index json-question))
                 :image-filename (rest (assoc :image--filename json-question))
                 :answer (rest (assoc :answer json-question))
                 :program (rest (assoc :program json-question))
                 :dataset-split (rest (assoc :split json-question))
                 :template-filename (rest (assoc :template--filename json-question))
                 :question-family-index (rest (assoc :question--family--index json-question))))
  
(defun read-questions-from-file (questions-file)
  "Read the clevr questions from a file. This functions expects a file
   where each line contains a json object of a question. If this is
   not the case, you can use json->lines in json-utils.lisp.
   Returns a list of 'clevr-question objects."
  (let ((questions
         (with-open-file (stream questions-file :direction :input)
           (mapcar #'decode-json-from-string
                   (stream->list stream)))))
    (mapcar #'process-json-question questions)))

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
                 :image-index (image-index obj-set)
                 :image-filename (image-filename obj-set)
                 :objects (mapcar #'copy-object (objects obj-set))))

(defmethod copy-object ((q clevr-question))
  (make-instance 'clevr-question :id (id q)
                 :question-index (question-index q) :question (question q)
                 :image-index (image-index q) :image-filename (image-filename q)
                 :answer (answer q) :program (program q) :dataset-split (dataset-split q)
                 :template-filename (template-filename q)
                 :question-family-index (question-family-index q)))
                 