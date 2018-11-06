;;;; /categories.lisp

(in-package :demo-wtnschp)

;; --------------
;; + Categories +
;; --------------

(defclass category (entity)
  ((prototype :accessor prototype :initarg :prototype :initform nil
              :documentation "The prototypical value for the category"))
  (:documentation "A general category"))

(defgeneric distance (category object)
  (:documentation "Distance measure between a category and an object"))

(defgeneric category->predicate (category var)
  (:documentation "Create an irl network (predicate) from the category"))

(defgeneric predicate->category (predicate)
  (:documentation "Get the category from an irl predicate"))

(defmethod predicate->category (predicate)
  (second predicate))

;; + Color Category +

(defclass color-category (category) ()
  (:documentation "Categories for the color feature"))

(defun make-color-category (prototype)
  (make-instance 'color-category
                 :prototype prototype))

(defmethod category->predicate ((cat color-category) var)
  `(:color ,(id cat) ,var))

(defmethod distance ((cat color-category) (obj sensory-object))
  (euclidean (prototype cat) (rgbcolor obj)))

(defmethod distance ((obj sensory-object) (cat color-category))
  (euclidean (prototype cat) (rgbcolor obj)))

(defmethod copy-object ((cat color-category))
  (make-instance 'color-category
                 :id (id cat)
                 :prototype (prototype cat)))

(defmethod print-object ((cat color-category) stream)
  (format stream "<color-category ~a>" (id cat)))
  