;;;; /world.lisp

(in-package :roos)

;; ------------------
;; + Sensory Object +
;; ------------------

(defclass sensory-object (entity)
  ((rgbcolor
    :type list :accessor rgbcolor :initform nil :initarg :rgbcolor
    :documentation "The color of the object in RGB")
   (features
    :type list :accessor features :initform nil :initarg :features
    :documentation "An a-list of features"))
  (:documentation "A sensory object"))

(defun alist->sensory-object (alist &key id)
  "Generate a sensory object from an alist (returned by decode-json)"
  (let ((obj (make-instance 'sensory-object :id (or id (make-id 'sensory-object)))))
    (setf (features obj) alist)
    (setf (rgbcolor obj) (rest (assoc :color alist)))
    obj))

(defmethod get-object-feature ((obj sensory-object) feature)
  "Get a feature of an object"
  (let ((pair (assoc feature (features obj))))
    (when pair
      (rest pair))))

(defmethod set-object-feature ((obj sensory-object) feature value)
  "Set a feature of an object"
  (let ((pair (assoc feature (features obj))))
    (when pair
      (rplacd pair value))))

(defmethod scale-object-feature ((obj sensory-object) feature min-val max-val)
  "Scale a feature of an object, given min and max bounds"
  (let* ((feature-val (get-object-feature obj feature))
         (scaled-val (if (listp feature-val)
                       (mapcar (lambda (x y) (float (/ x y)))
                               (mapcar #'- feature-val min-val)
                               (mapcar #'- max-val min-val))
                       (float (/ (- feature-val min-val)
                                 (- max-val min-val))))))
    (set-object-feature obj feature scaled-val)))

(defmethod copy-object ((obj sensory-object))
  (make-instance 'sensory-object
                 :id (id obj)
                 :rgbcolor (copy-object (rgbcolor obj))
                 :features (copy-alist (features obj))))

(defmethod print-object ((obj sensory-object) stream)
  (format stream "<sensory-object: ~a>" (id obj)))

;; --------------
;; + Object Set +
;; --------------

(defclass object-set (entity)
  ((entities :accessor entities :initarg :entities :initform nil :type list
             :documentation "The entities of the set"))
  (:documentation "A set of sensory objects"))

(defun make-object-set (entities &key id)
  "Make an entity set from a list of entities"
  (declare (type list entities))
  (make-instance 'object-set
                 :entities entities
                 :id (or id (make-id 'entity-set))))

(defmethod equal-entity ((set-1 object-set) (set-2 object-set))
  "Two sets are equal when their components are equal"
  (permutation-of? (entities set-1) (entities set-2) :test #'equal-entity))

(defmethod find-entity-by-id ((set object-set) (id symbol))
  "Use ID to find an entity in a set"
  (if (eq (id set) id)
    set
    (find id (entities set) :key #'id)))

(defmethod all-entities-but-id ((set object-set) (id symbol))
  "Return all entities in the set, but the one with the given id"
  (remove id (entities set) :key #'id))

(defun alist->object-set (alist)
  "Create an object set from a given a-list (returned by decode-json)"
  (let ((ids (mapcar #'(lambda (lst) (first (first lst))) alist))
        (features (mapcar #'(lambda (lst) (rest (first lst))) alist))
        entities)
    (loop for (id . features) in (pairlis ids features)
          for object = (alist->sensory-object features :id id)
          do (push object entities))
    (make-object-set entities)))

(defmethod copy-object ((set object-set))
  (make-instance 'object-set
                 :id (id set)
                 :entities (mapcar #'copy-object (entities set))))

(defmethod print-object ((set object-set) stream)
  (format stream "<object-set: ~a>" (id set)))