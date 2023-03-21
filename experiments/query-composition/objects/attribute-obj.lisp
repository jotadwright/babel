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
    :accessor type-att)
   (constraint
    :initarg :constraint
    :initform nil
    :accessor constraint))
  (:documentation "An object representing an attribute of a table. It is composed of its name, the set of operators that can be applied to it and the type of data it will contain."))

(defun define-type (att obj)
  (cond ((typep att 'string)
         (progn
           (setf (operators obj) '(:!= :=))
           (setf (type-att obj) 'string)))
        ((and (typep att 'integer) (not (or (equal (constraint obj) 'foreign) (equal (constraint obj) 'primary))))
         (progn
           (setf (operators obj) '(:< :> :<= :>= :!= :=))
           (setf (type-att obj) 'integer)))
        ((or (equal (constraint obj) 'foreign) (equal (constraint obj) 'primary))
         (progn
           (setf (operators obj) '(:!= :=))
           (setf (type-att obj) 'integer)))
        ((typep att 'boolean)
         (progn
           (setf (operators obj) '(:!= :=))
           (setf (type-att obj) 'boolean)))))
          
    

(defun define-constraint (table-name obj)
  (let* ((primary "SELECT tc.table_name, kcu.column_name FROM  information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name WHERE constraint_type = 'PRIMARY KEY' AND tc.table_name=")
        (foreign "SELECT tc.table_name, kcu.column_name FROM  information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name WHERE constraint_type = 'FOREIGN KEY' AND tc.table_name=")
        (primary-result (query (concatenate 'string primary "'" table-name "'")))
        (foreign-result (query (concatenate 'string foreign "'" table-name "'" ))))
     (mapcar #'(lambda (x) (if (equal (nth 1 x) (name obj)) (setf (constraint obj) 'primary))) primary-result)
     (mapcar #'(lambda (x) (if (equal (nth 1 x) (name obj)) (setf (constraint obj) 'foreign))) foreign-result)))