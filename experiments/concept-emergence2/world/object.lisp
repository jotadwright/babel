(in-package :cle)

;; ------------------
;; + CLE object set +
;; ------------------

(defmethod s-expr->cle-scene (s-expr world view-name)
  "Create an instance of cle-scene from an s-expression"
  (let* ((view (get-view world view-name))
         (categorical-features (get-categorical-features world view-name)))
    (make-instance 'cle-scene
                   :index (gethash :image_index s-expr)
                   :dataset (view-name view)
                   :dataset-split (dataset-split view)
                   :image-fname (gethash :image_filename s-expr)
                   :objects (loop for obj being the elements of (gethash :objects s-expr) ;; objects is a vector
                                  collect (s-expr->cle-object obj (feature-set view) categorical-features)))))

(defmethod objects->cle-scene (objects world view-name)
  "Create an instance of cle-scene from an s-expression"
  (let ((view (get-view world view-name)))
    (make-instance 'cle-scene
                   :index 0
                   :dataset (view-name view)
                   :dataset-split (dataset-split view)
                   :image-fname "nil"
                   :objects objects)))

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
(defmethod s-expr->cle-objects (s-expressions feature-set categorical-features)
  (loop for s-expr in s-expressions
        for cle-object = (s-expr->cle-object s-expr feature-set categorical-features)
        collect cle-object))

(defmethod s-expr->cle-object (s-expr feature-set categorical-features)
  "Create an instance of cle-scene from an s-expression"
  (let* ((description (gethash :description s-expr))
         ;; convert categorical data (strings) to symbols for efficient comparisons
         (data (convert-object-strings-to-symbols (gethash :attributes s-expr) categorical-features))
         ;; filter the raw s-expr on only the available feature-set
         (attributes (filter-objects-on-features data feature-set)))
    (make-instance 'cle-object
                   :attributes attributes
                   :description description)))

(defclass cle-object (entity)
  ((id
    :initarg :id :accessor id :initform (make-id "OBJ") :type symbol
    :documentation "Id of the object.")
   (attributes
    :documentation "The attributes of the object."
    :type hash-table :accessor attributes :initarg :attributes)
   (description
    :documentation "Symbolic description of the object."
    :type list :accessor description :initarg :description)))

(defun has-topic-id (cle-object)
  (gethash :id (description cle-object)))

(defun get-topic-id (cle-object)
  (gethash :id (description cle-object)))

;; ------------------------
;; + Small utils channels +
;; ------------------------
(defmethod get-object-val ((object cle-object) (attr symbol))
  (gethash attr (attributes object)))

(defmethod convert-object-strings-to-symbols (object categorical-features)
  (loop for feature in categorical-features
        do (setf (gethash feature object) (parse-keyword (gethash feature object)))
        finally (return object)))
                                           
(defun filter-objects-on-features (object feature-set)
  "Only keep the attributes that are in play."
  (loop for feature being the hash-keys of object
        if (not (member feature feature-set :test #'equalp))
           do (remhash feature object)
        finally (return object)))

(defmethod print-object ((cle-object cle-object) stream)
  (pprint-logical-block (stream nil)
    (format stream "<~a:~
                        ~:_ attributes: ~{~,2f~^, ~}"
            (id cle-object)
            (sort (loop for channel being the hash-keys of (attributes cle-object)
                          using (hash-value value)
                        collect (cons channel value))
                  (lambda (x y) (string< (symbol-name x) (symbol-name y)))
                  :key #'car))
    (format stream ">")))
