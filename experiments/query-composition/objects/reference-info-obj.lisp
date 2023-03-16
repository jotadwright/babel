(in-package :qc)

(defclass reference-info ()
  ((table-name
    :type string
    :initarg :table-name
    :initform nil
    :accessor table-name)
   (column-name
    :type string
    :initarg :column-name
    :initform nil
    :accessor column-name)
   (foreign-table
    :type string
    :initarg :foreign-table
    :initform nil
    :accessor foreign-table)
   (foreign-column
    :type string
    :initarg :foreign-column
    :initform nil
    :accessor foreign-column)))


(defun init-reference-info (lst)
  (make-instance 'reference-info :table-name (nth 0 lst)  :column-name (nth 1 lst) :foreign-table (nth 2 lst) :foreign-column (nth 3 lst)))