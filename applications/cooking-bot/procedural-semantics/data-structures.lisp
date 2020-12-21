(in-package :cooking-bot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; This file contains the datastructures and basic methods for      ;;
;; the procedural semantics environment.                            ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; primitive inventory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass primitive-inventory ()
  ((primitives :type list :accessor primitives :initarg :primitives :initform nil))
  (:documentation "The inventory of primitives."))


;; variables ;;
;;;;;;;;;;;;;;;

(defclass var ()
  ((name :type symbol :accessor name :initarg :name :initform (make-id 'var)))
  (:documentation "Represents a variable"))

(defmethod copy-object-content ((source var) (destination var))
  (setf (name destination) (name source)))

(defmethod print-object ((var var) stream)
  (format stream "?~a" (name var)))

(defmacro var (&optional name)
  `(make-var (when ',name (make-id ',name))))


;; bindings ;;
;;;;;;;;;;;;;;

(defclass bindings-list ()
  ((status :type symbol :accessor status :initarg :status :initform t)
   (bindings :type list :accessor bindings :initarg :bindings :initform nil)))

(defmethod copy-object-content ((source bindings-list) (destination bindings-list))
  (setf (status destination) (status source))
  (setf (bindings destination) (loop for el in (bindings source)
                                     collect (copy-object el))))

(defmethod print-object ((bindings-list bindings-list) stream)
  (format stream "<bindings-list: ~a>" (cond ((not (status bindings-list))
                                              "FAILED")
                                             ((not (bindings bindings-list))
                                              "SUCCEEDED")
                                             (t
                                              (format nil "(~{~a~^ ~})" (bindings bindings-list))))))

(defmethod fail-bindings-list ((bindings-list bindings-list))
  (setf (status bindings-list) nil))

(defmethod failed-p ((bindings-list bindings-list))
  (not (status bindings-list)))

(defmethod succeeded-p ((bindings-list bindings-list))
  (status bindings-list))

(defmethod lookup-binding ((var var) (bindings-list bindings-list))
  (loop with var-name = (name var)
        for binding in (bindings bindings-list)
        when (equalp (name (binding-var binding)) var-name)
        return binding))

(defmethod extend-bindings-list ((binding binding) (bindings-list bindings-list))
  (let ((existing-binding? (lookup-binding (binding-var binding) bindings-list)))
    (cond ((not existing-binding?)
           (push binding (bindings bindings-list))
           bindings-list)
          ((equal-ontology-objects (binding-value existing-binding?)
                                   (binding-value binding))
           bindings-list)
          (t
           (fail-bindings-list bindings-list)))))

(defclass binding ()
  ((binding-var :type var :accessor binding-var :initarg :binding-var :initform (var))
   (binding-value :type t :accessor binding-value :initarg :binding-value :initform nil)
   (binding-score :type number :accessor binding-score :initarg :binding-score :initform 1.0)))

(defmethod copy-object-content ((source binding) (destination binding))
  (setf (binding-var destination) (copy-object (binding-var source)))
  (setf (binding-value destination) (copy-ontology-object (binding-value source)))
  (setf (binding-score destination)  (binding-score source)))

(defmethod print-object ((binding binding) stream)
  (format stream "<~a . ~a (~a)>" (binding-var binding) (binding-value binding) (binding-score binding)))

;; Concepts ;;
;;;;;;;;;;;;;;

(defgeneric concept-of (ontological-class)
  (:documentation "Returns the concept (prototype instance of the class."))

(defmethod concept-of ((ontological-class symbol))
  (harlequin-common-lisp:class-prototype ontological-class))


;; predicates ;;
;;;;;;;;;;;;;;;;

(defclass predicate-network ()
  ((predicates :type list :accessor predicates :initarg :predicates :initform nil))
  (:documentation "Represents a predicate network"))

(defmethod print-object ((predicate-network predicate-network) stream)
  (format stream "<predicate-network: ~{~a~^ ~}>" (predicates predicate-network)))

(defmacro def-predicate-network (predicates)
  `(do-def-predicate-network ',predicates))

(defun do-def-predicate-network (list-of-predicates)
  (make-instance 'predicate-network
                 :predicates (loop for predicate in list-of-predicates
                                   collect (do-def-predicate predicate))))

(defmacro def-predicate (predicate)
  `(do-def-predicate ',predicate))

(defun do-def-predicate (predicate)
  (loop with predicate-name = (first predicate)
        for arg in (rest predicate)
        if (numberp arg)
        collect arg into args
        else if (string= (subseq (symbol-name arg) 0 1) "?")
        collect (make-instance 'var :name (subseq (symbol-name arg) 1)) into args
        else
        collect (concept-of arg) into args
        finally return (cons predicate-name args)))
