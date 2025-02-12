(in-package :concept-representations)

;; -----------
;; + Entity +
;; -----------

(defclass entity ()
  ((id
    :initarg :id :accessor id :initform (make-id "ENTITY") :type symbol
    :documentation "Id of the entity.")
   (features
     :initarg :features :accessor features :type hash-table
     :documentation "Numerical or continuous features of the entity.")
   (description
    :initarg :description :accessor description :type hash-table
    :documentation "Symbolic descriptions of the entity"))
  (:documentation "Class for entity."))


(defmethod get-feature-value ((entity entity) (feature-name symbol))
  "Getter for value of a feature in an entity given the name."
  (gethash feature-name entity))