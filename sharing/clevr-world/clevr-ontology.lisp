(in-package :clevr-world)

;; ################################
;; clevr categories
;; ################################

(export '(category attribute shape-category size-category
          color-category material-category spatial-relation-category
          spatial-relation boolean-category bool
          attribute-category attribute attention img-path
          shapes sizes colors materials spatial-relations attributes
          category-value))

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

(defclass attention (entity)
  ((img-path :type (or null pathname) :initarg :img-path
             :accessor img-path :initform nil))
  (:documentation "A symbolic representation of an intermediate attention"))

;; ################################
;; category-value
;; ################################

(defgeneric category-value (category)
  (:documentation "Obtain the value of the category"))

(defmethod category-value ((shape-category shape-category))
  (shape shape-category))
(defmethod category-value ((size-category size-category))
  (size size-category))
(defmethod category-value ((color-category color-category))
  (color color-category))
(defmethod category-value ((material-category material-category))
  (material material-category))
(defmethod category-value ((spatial-relation-category spatial-relation-category))
  (spatial-relation spatial-relation-category))
(defmethod category-value ((boolean-category boolean-category))
  (bool boolean-category))
(defmethod category-value ((attribute-category attribute-category))
  (attribute attribute-category))
(defmethod category-value ((attention attention))
  (id attention))

;; ################################
;; clevr ontology
;; ################################

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
  (let* ((metadata-file (babel-pathname :directory '("sharing" "clevr-world" "data")
                                        :name "metadata" :type "json"))
         (metadata (decode-json-as-alist-from-source metadata-file))
         (metadata-types (cdr (assoc :types metadata)))
         (clevr-ontology (make-blackboard)))
    (loop for (category-key . vocab) in metadata-types
          for category = (internal-symb (upcase (mkstr category-key)))
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

;; ################################
;; global variables
;; ################################

(export '(*clevr-ontology*))

(defparameter *clevr-ontology* (build-clevr-ontology))

;; ################################
;; copy object
;; ################################

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

(defmethod copy-object ((attention attention))
  (make-instance 'attention :id (id attention)))