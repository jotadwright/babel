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
    :accessor type-att)))

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
        
        
(defun get-permutations (attributes)
  (let
                  

