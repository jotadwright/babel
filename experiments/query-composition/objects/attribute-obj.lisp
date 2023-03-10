(in-package :qc)

(defclass attribute ()
  ((name
    :initarg :name
    :type string
    :accessor name)
   (operators
    :initarg :operators
    :initform '()
    :accessor operators)
   (type-att
    :initarg :type-att
    :initform nil
    :accessor type-att))
  (:documentation "An object representing an attribute of a table. It is composed of its name, the set of operators that can be applied to it and the type of data it will contain."))

(defun define-type (att obj)
  (cond ((typep att 'string)
         (progn
           (setf (operators obj) '(:!= :=))
           (setf (type-att obj) 'string)))
        ((typep att 'integer)
         (progn
           (setf (operators obj) '(:< :> :<= :>= :!= :=))
           (setf (type-att obj) 'integer)))
        ((typep att 'boolean)
         (progn
           (setf (operators obj) '(:!= :=))
           (setf (type-att obj) 'boolean)))))

