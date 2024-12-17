(in-package :cle)

;; ------------------
;; + CLE object set +
;; ------------------

(defmethod s-expr->cle-scene (s-expr world)
  "Create an instance of cle-scene from an s-expression"
  (make-instance 'cle-scene
                 :index (rest (assoc :image--index s-expr))
                 :dataset (dataset-name world)
                 :dataset-split (dataset-split world)
                 :image-fname (rest (assoc :image--filename s-expr))
                 :objects (loop for obj in (rest (assoc :objects s-expr))
                                collect (s-expr->cle-object obj (feature-set world)))))

(defmethod objects->cle-scene (objects world)
  "Create an instance of cle-scene from an s-expression"
  (make-instance 'cle-scene
                 :index 0
                 :dataset (dataset-name world)
                 :dataset-split (dataset-split world)
                 :image-fname "nil"
                 :objects objects))

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
(defmethod s-expr->cle-objects (s-expressions feature-set)
  (loop for s-expr in s-expressions
        for idx = (parse-integer (symbol-name (first s-expr)))
        for cle-object = (s-expr->cle-object (rest s-expr) feature-set)
        collect cle-object))

(defmethod s-expr->cle-object (s-expr feature-set)
  "Create an instance of cle-scene from an s-expression"
  (make-instance 'cle-object
                 ;; filter the raw s-expr on only the available feature-set
                 :attributes (filter-object (intern-alist (rest (assoc :attributes s-expr))) feature-set)
                 :description (rest (assoc :description s-expr))))

(defclass cle-object (entity)
  ((id
    :initarg :id :accessor id :initform (make-id "OBJ") :type symbol
    :documentation "id of the object.")
   (attributes
    :documentation "The attributes of the object (a-list)."
    :type hash-table :accessor attributes :initarg :attributes)
   (description
    :documentation "Symbolic description of the original object."
    :type list :accessor description :initarg :description)))

;; ------------------------
;; + Small utils channels +
;; ------------------------
(defmethod get-object-val ((object cle-object) (attr symbol))
  (gethash attr (attributes object)))

(defun intern-alist (alist)
  (mapcar #'(lambda (pair)
              (cons (intern (upcase (mkstr (car pair))) :cle)
                    (cdr pair)))
          alist))

(defun filter-object (object feature-set)
  "Only keep the attributes that are in play."
  (loop with hash-table = (make-hash-table)
        for channel in feature-set
        do (setf (gethash channel hash-table) (assqv channel object))
        finally (return hash-table)))

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
