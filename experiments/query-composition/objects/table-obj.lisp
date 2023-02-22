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


(defun init-table (table-name attrs)
  (let ((table (make-instance 'table :name table-name :attributes '())))
    (dolist (attr attrs)
      (push (make-instance 'attribute :name (concat-array attr)) (attributes table)))
    table))

(defun init-schema ()
  ;(connect-toplevel "master_db" "postgres" "root" "localhost")
  (let ((tables '())
        (table-name nil)
        (attrs '()))
    (dolist (res (query "SELECT table_name, column_name FROM information_schema.columns WHERE table_schema='public' ORDER BY table_name;"))
      (if (not table-name)
        (setf table-name (first res)))
      (if (equal table-name (first res))
        (push (last res) attrs)
        (progn
          (push (init-table table-name attrs) tables)
          (setf attrs '())
          (setf table-name (first res)))))
    (push (init-table table-name attrs) tables)
  tables))



(init-schema)