(in-package :concept-representations)

;; -----------
;; + Entity +
;; -----------

(defclass entity ()
  ((id
    :initarg :id :accessor id :initform (make-id "ENTITY") :type symbol
    :documentation "Id of the entity.")
   (features
     :initarg :features :accessor features :type hash-table)
   (description
    :initarg :description :accessor description :type hash-table))
  (:documentation "...."))


(defmethod get-feature-value ((entity entity) (feature-name symbol))
  (gethash feature-name entity))