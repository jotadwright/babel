(in-package :duckie-language-learning)

;; sets of objects in the world
(defclass object-set (entity)
  ((objects
    :initarg :objects :accessor objects :initform nil :type list
    :documentation "a set of objects"))
  (:documentation "a set of objects in the duckie-world"))

(defmethod make-html-for-entity-details ((set object-set) &key)
  `(((div :class "entity-detail") 
     ,@(loop for object in (objects set)
             collect (make-html object :expand-initially t)))))

(defmethod equal-entity ((set-1 object-set) (set-2 object-set))
  (permutation-of? (objects set-1) (objects set-2) :test #'equal-entity))