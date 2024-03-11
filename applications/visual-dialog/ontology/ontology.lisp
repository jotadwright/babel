(in-package :visual-dialog)

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

(defclass 2D-relation-category (category)
  ((2D-relation :type symbol :initarg :2D-relation :reader 2D-relation))
  (:documentation "A spatial relation category"))

(defclass boolean-category (category)
  ((bool :type bool :initarg :bool :reader bool))
  (:documentation "An entity representation of booleans"))

(defclass attribute-category (category)
  ((attribute :type symbol :initarg :attribute :reader attribute))
  (:documentation "A category to represent object attributes"))

(defclass digit-category (attribute)
  ((digit :type (or number symbol) :initarg :digit :reader digit))
  (:documentation "A digit category"))

(defclass bgcolor-category (attribute)
  ((bgcolor :type symbol :initarg :bgcolor :reader bgcolor))
  (:documentation "A material category"))

(defclass style-category (attribute)
  ((style :type symbol :initarg :style :reader style))
  (:documentation "A style category"))


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
(defmethod add-category-to-ontology (ontology value (category (eql 'style)))
  (push-data ontology 'styles (make-instance 'style-category :id value :style value)))
(defmethod add-category-to-ontology (ontology value (category (eql 'relation)))
  (push-data ontology 'spatial-relations (make-instance 'spatial-relation-category :id value :spatial-relation value)))
(defmethod add-category-to-ontology (ontology value (category (eql '2D-relation)))
  (push-data ontology '2D-relations (make-instance '2D-relation-category :id value :2D-relation value)))
(defmethod add-category-to-ontology (ontology value (category (eql 'attribute)))
  (push-data ontology 'attributes (make-instance 'attribute-category :id value :attribute value)))
(defmethod add-category-to-ontology (ontology value (category (eql 'digit)))
  (push-data ontology 'digits (make-instance 'digit-category :id value :digit value)))
(defmethod add-category-to-ontology (ontology value (category (eql 'bgcolor)))
  (push-data ontology 'bgcolors (make-instance 'bgcolor-category :id value :bgcolor value)))

(defun build-ontology ()
  "Build the  ontology from the metadata file."
  (let* ((metadata-file (babel-pathname :directory '("sharing" "clevr-world" "data")
                                        :name "metadata" :type "json"))
         (metadata (decode-json-from-source metadata-file))
         (metadata-types (cdr (assoc :types metadata)))
         (ontology (make-blackboard)))
    (loop for (category-key . vocab) in metadata-types
          for category = (internal-symb (upcase (subseq (mkstr category-key) 1)))
          when vocab
          do (loop for value-str in vocab
                   for value = (internal-symb (upcase value-str))
                   do (add-category-to-ontology ontology value category)))
    ;; manually add 'thing' as a shape
    (add-category-to-ontology ontology 'thing 'shape)
    ;; manually add 'none as shape, color, material and size
    (add-category-to-ontology ontology 'none 'shape)
    (add-category-to-ontology ontology 'none 'color)
    (add-category-to-ontology ontology 'none 'material)
    (add-category-to-ontology ontology 'none 'size)
    (add-category-to-ontology ontology 'none 'digit)
    (add-category-to-ontology ontology 'none 'style)
    (add-category-to-ontology ontology 'none 'bgcolor)
    ;;manually add colors, styles and 2D relations from mnist-dialog
    (add-category-to-ontology ontology 'salmon-bg 'bgcolor)
    (add-category-to-ontology ontology 'cyan-bg 'bgcolor)
    (add-category-to-ontology ontology 'yellow-bg 'bgcolor)
    (add-category-to-ontology ontology 'silver-bg 'bgcolor)
    (add-category-to-ontology ontology 'white-bg 'bgcolor)
    (add-category-to-ontology ontology 'violet 'color)
    (add-category-to-ontology ontology 'number 'digit)
    (add-category-to-ontology ontology 'below '2D-relation)
    (add-category-to-ontology ontology 'above '2D-relation)
    (add-category-to-ontology ontology '2D-right '2D-relation)
    (add-category-to-ontology ontology '2D-left '2D-relation)
    (add-category-to-ontology ontology 'flat 'style)
    (add-category-to-ontology ontology 'stroke 'style)
    ;; manually add 'center as spatial-relation
    (add-category-to-ontology ontology 'center 'relation)
    (add-category-to-ontology ontology 'below '2D-relation)
    (add-category-to-ontology ontology 'above '2D-relation)
    ;; manually add true and false to the ontology
    (push-data ontology 'booleans (make-instance 'boolean-category :id 'yes :bool t))
    (push-data ontology 'booleans (make-instance 'boolean-category :id 'no :bool nil))
    ;; manually add all attributes to the ontology
    (loop for attribute in '(shape size material color timestamp digit bgcolor style number)
          do (add-category-to-ontology ontology attribute 'attribute))
    ;;manually add number
    (loop for int from 0 to 50
          do (push-data ontology 'digits
                        (make-instance 'digit-category :id (internal-symb (upcase (format nil "~r" int))) 
                                       :digit int)))
    ontology))
