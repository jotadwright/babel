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

;; + Category Set +

(defclass category-set (entity)
  ((entities :accessor entities :initarg :entities :initform nil
             :documentation "The members of the set"))
  (:documentation "A set of categories"))

(defun make-category-set (entities &key id)
  "Make an entity set from a list of entities"
  (declare (type list entities))
  (make-instance 'category-set
                 :entities entities
                 :id (or id (make-id 'category-set))))

(defmethod equal-entity ((set-1 category-set) (set-2 category-set))
  "Two sets are equal when their components are equal"
  (permutation-of? (entities set-1) (entities set-2) :test #'equal-entity))

(defmethod find-entity-by-id ((set category-set) (id symbol))
  "Use ID to find an entity in a set"
  (if (eq (id set) id)
    set
    (find id (entities set) :key #'id)))

(defmethod all-entities-but-id ((set category-set) (id symbol))
  "Return all entities in the set, but the one with the given id"
  (remove id (entities set) :key #'id))

(defmethod copy-object ((set category-set))
  (make-instance 'category-set
                 :id (id set)
                 :entities (mapcar #'copy-object (entities set))))

(defmethod print-object ((set category-set) stream)
  (format stream "<category-set: ~a>" (id set)))
  