(in-package :qc)

(defclass table ()
  ((name
    :type string
    :initarg :name
    :accessor name)
   (attributes
    :type list
    :initform nil
    :initarg :attributes
    :accessor attributes)))


(defun init-table (lst)
  (let ((table (make-instance 'table :name (first (first lst)) :attributes '())))
    (dolist (row lst)
      (push (make-instance 'attribute :name (concat-array (last row))) (attributes table)))
    table))