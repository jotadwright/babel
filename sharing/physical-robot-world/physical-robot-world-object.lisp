(in-package :physical-robot-world)

;; ############################################################################
;; physical-robot-world-object
;; ############################################################################

(export '(physical-robot-world-object object features object-type get-type
          add-feature find-feature find-fvalue get-fvalue get-fvalue-value
          robot-p object-p box-p region-p))

(defclass physical-robot-world-object ()
  ((id :type symbol :initarg :id :accessor id)
   (features :initform nil :type list :initarg :features :accessor features
             :documentation "The a-list of features for this object."))
  (:documentation "Physical robot world object class"))

(defmethod object->s-expr ((object physical-robot-world-object))
  (list 'object (id object) (features object)))

(defmethod s-expr->object ((type (eql 'physical-robot-world-object))
                           (s-expr list) &key)
  "Makes physical-robot-world-objects from s-expr
  like (<type> <id> ((<fname> (<feature-type> <value>))
                     (<fname> (<feature-type> <value>))"
  (destructuring-bind (object-type id . args) s-expr
    (assert (eq object-type 'object))
    (make-instance 'physical-robot-world-object
                   :id id
                   :features (loop for expr in (car args)
                                   collect (cons (car expr) (cadadr expr))))))

(defmethod copy-object ((o physical-robot-world-object)) o)

(defmethod print-object ((object physical-robot-world-object) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "~(~:w~)" (object->s-expr object)))
      (call-next-method)))

;; ############################################################################
;; helper functions for accessing features of an object
;; ############################################################################

(defun add-feature (object feature)
  "Add the given feature to the given entity."
  (declare (type physical-robot-world-object object)
	   (type feature cons)
           (type (car feature) symbol))
  ;; assert that there are no two features with the same name
  (assert (not (member (car feature) (features object) :test #'eq :key #'car)) ()
          "Error while trying to add new feature ~a~
           ~%There is already a feature with name ~(~a~) in ~a"
          feature (car feature) object)
  (push feature (features object)))

(defun find-feature (object name)
  "Return the first feature with the given id
   or nil if there is no feature with the
   given id."
  (declare (type physical-robot-world-object object)
           (type symbol name))
  (assoc name (features object)))

(defun find-fvalue (object name)
  "Return the value of the feature with the given id,
   or nil if there is no such feature."
  (declare (type physical-robot-world-object object)
           (type symbol name))
  (let ((found (find-feature object name)))
    (when found (cdr found))))

(defun get-fvalue (object name)
  "Return the value of the feature with the given id, or signal an error if
    there is no feature with the given id."
  (declare (type physical-robot-world-object object)
           (type symbol name))
  (let ((feature (find-feature object name)))
    (if feature
        (cdr feature)
        (error "There is no feature with id ~(~a~) in ~(~a~)."
               name object))))

;; ############################################################################
;; helper functions for checking the type of an object
;; ############################################################################

(defun robot-p (object)
  "returns t when an object is a robot"
  (declare (type physical-robot-world-object object))
  (let ((fvalue (find-fvalue object 'image-schema)))
    (when fvalue
      (eq fvalue 'robot))))

(defun box-p (object)
  "returns t when an object is a box"
  (declare (type physical-robot-world-object object))
  (let ((fvalue (find-fvalue object 'image-schema)))
    (when fvalue
      (eq fvalue 'box))))

(defun region-p (object)
  "returns t when an object is a region"
  (declare (type physical-robot-world-object object))
  (let ((fvalue (find-fvalue object 'image-schema)))
    (when fvalue
      (eq fvalue 'region))))

(defun get-type (object)
  "returns the image-schema of the object"
  (declare (type physical-robot-world-object object))
  (let ((fvalue (find-fvalue object 'image-schema)))
    (if fvalue fvalue 'object)))

(defun object-p (object)
  "returns t when an object is an object"
  (declare (type physical-robot-world-object object))
  (not (or (region-p object)
           (box-p object)
           (robot-p object))))
