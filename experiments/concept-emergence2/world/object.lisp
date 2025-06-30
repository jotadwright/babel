(in-package :cle)

;; ------------------
;; + CLE object set +
;; ------------------

(defmethod data->cle-scene (data world view-name)
  "Create an instance of cle-scene from a hash-table data structure"
  (let* ((view (get-view world view-name))
         (categorical-features (get-categorical-features world view-name)))
    (make-instance 'cle-scene
                   :index (gethash :image_index data)
                   :dataset (view-name view)
                   :dataset-split (dataset-split view)
                   :image-fname (gethash :image_filename data)
                   :entities (loop for obj being the elements of (gethash :objects data) ;; objects is a vector
                                  collect (create-entity obj (feature-set view) categorical-features)))))

(defmethod entities->cle-scene (entities world view-name)
  "Create an instance of cle-scene from a list of cle-objects"
  (let ((view (get-view world view-name)))
    (make-instance 'cle-scene
                   :index 0
                   :dataset (view-name view)
                   :dataset-split (dataset-split view)
                   :image-fname "nil"
                   :entities entities)))

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
   (entities
    :documentation "The entities in the set."
    :type list :accessor entities :initarg :entities
    :initform nil))
  (:documentation "A set of entities."))

;; ------------------------
;; + Small utils channels +
;; ------------------------

#|(defmethod print-object ((cle-object cle-object) stream)
  (pprint-logical-block (stream nil)
    (format stream "<~a:~
                        ~:_ features: ~{~,2f~^, ~}"
            (id cle-object)
            (sort (loop for channel being the hash-keys of (features cle-object)
                          using (hash-value value)
                        collect (cons channel value))
                  (lambda (x y) (string< (symbol-name x) (symbol-name y)))
                  :key #'car))
    (format stream ">")))|#