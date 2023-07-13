(in-package :cle)

;; ------------------
;; + CLE object set +
;; ------------------

(defmethod s-expr->cle-scene (s-expr &key dataset dataset-split available-channels)
  "Create an instance of cle-scene from an s-expression"
  (make-instance 'cle-scene
                 :index (rest (assoc :image--index s-expr))
                 :dataset dataset
                 :dataset-split dataset-split
                 :image-fname (rest (assoc :image--filename s-expr))
                 :objects (loop for obj in (rest (assoc :objects s-expr))
                                collect (s-expr->cle-object obj available-channels))))

(defclass cle-scene (entity)
  ((index
    :documentation "Index of the scene"
    :type int :accessor index :initarg :index)
   (dataset
    :documentation "Top level dataset of the scene"
    :type string :accessor dataset :initarg :dataset)
   (dataset-split
    :documentation "Split of the dataset"
    :type string :accessor dataset-split :initarg :dataset-split)
   (image-fname
    :documentation "Path of the image of this set."
    :type str :accessor image-fname :initarg :image-fname)
   (objects
    :documentation "The objects in the set."
    :type list :accessor objects :initarg :objects
    :initform nil))
  (:documentation "A set of cle-objects."))

;; --------------
;; + CLE-Object +
;; --------------
(defmethod s-expr->cle-object (s-expr available-channels)
  "Create an instance of cle-scene from an s-expression"
  (make-instance 'cle-object
                 ;; filter the raw s-expr on only the available channels
                 :attributes (filter-object (intern-alist (rest (assoc :attributes s-expr))) available-channels)
                 :description (rest (assoc :description s-expr))))

(defclass cle-object (entity)
  ((attributes
    :documentation "The attributes of the object (a-list)."
    :type list :accessor attributes :initarg :attributes)
   (description
    :documentation "Symbolic description of the original object."
    :type list :accessor description :initarg :description)))

;; --------------------------------
;; + Small utils channels +
;; --------------------------------

(defun intern-alist (alist)
  (mapcar #'(lambda (pair)
              (cons (intern (upcase (mkstr (car pair))) :cle)
                    (cdr pair)))
          alist))

(defun filter-object (object available-channels)
  "Only keep the attributes that are in play."
  (loop for channel in available-channels
        collect (assoc channel object)))

(defmethod print-object ((cle-object cle-object) stream)
  (pprint-logical-block (stream nil)
    (format stream "<cle-object:~
                        ~:_ attributes: ~{~,2f~^, ~}"
            (reverse (attributes cle-object)))
    (format stream ">")))
