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

(defmethod print-object ((var var) stream)
  (format stream "?~a" (name var)))

(defmacro var (&optional name)
  `(make-var (when ',name (make-id ',name))))


;; bindings ;;
;;;;;;;;;;;;;;

(defclass binding ()
  ((binding-var :type var :accessor binding-var :initarg :binding-var :initform (var))
   (binding-value :type t :accessor binding-value :initarg :binding-value :initform nil)
   (binding-score :type number :accessor binding-score :initarg :binding-score :initform 1.0)))

(defmethod print-object ((binding binding) stream)
  (format stream "<~a . ~a (~a)>" (binding-var binding) (binding-value binding) (binding-score binding)))


;; concepts ;;
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
  (format stream "<predicate-network: ~{~a ~^~}>" (predicates predicate-network)))

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
