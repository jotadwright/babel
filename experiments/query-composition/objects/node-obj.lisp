(in-package :qc)

(defclass node ()
  ((id :type integer :initarg :id :accessor id)
   (parent :type node :initarg :parent :initform nil :accessor parent)
   (children :type list :initarg :children :initform '() :accessor children)
   (depth :type integer :initarg :depth :initform nil :accessor depth)
   (tble :type table :initarg :tble :initform nil :accessor tble)
   (ref-tbles :type list :initarg :ref-tbles :initform '() :accessor ref-tbles)
   (attrs :type attrs :initarg :attrs :initform nil :accessor attrs)
   (time-result :type float :initarg :time-result :initform nil :accessor time-result)
   (q :type integer :initarg :q :initform "" :accessor q)))

;;OK
(defun init-node (node attributes table &key join)
  "function that create a node with the SELECT .. FROM .. clause and return the newly created node with its associated parent."
    (let* ((q '(:select))
           (child (make-instance 'node :id (make-id) :parent node :depth (+ (depth node) 1)  :q "" :tble table :ref-tbles (list table)))
           (last-elem (last attributes))
           (table-name (intern (name table))))
      (dolist (att-n attributes)
          (if join
            (progn
              (let ((att (intern (concatenate 'string (symbol-name table-name) "." att-n))))
                (setf q (push-end att q))))
            (progn
               (setf q (push-end (intern att-n) q)))))
      (setf q (push-end :from q))
      (setf q (push-end table-name q))
      (setf (q child) q)
      child))

;;OK
(defun join-node (node ref-info table &key foreign-ref outer-join)
  "function that create a node with the ... INNER JOIN || OUTER JOIN ... ON ... =  ... clause and return the newly created node."
  (let* ((f-table "")
          (f-column "")
          (table "")
          (column "")
          (join :inner-join))
    (if foreign-ref
      (progn
        (setf f-table (foreign-table ref-info))
        (setf f-column (foreign-column ref-info))
        (setf table (table-name ref-info))
        (setf column (column-name ref-info)))
      (progn
        (setf f-table (table-name ref-info))
        (setf f-column (column-name ref-info))
        (setf table (foreign-table ref-info))
        (setf column (foreign-column ref-info))))
    ;(if outer-join
    ;  (setf join " FULL OUTER JOIN "))
    (let* ((foreign-t (intern (concatenate 'string f-table "." f-column)))
            (main-t (intern (concatenate 'string table "." column)))
           (join-query (list join (intern f-table) :on (list := foreign-t main-t))))
  (make-instance 'node :id (make-id) :parent node :depth (depth node) :ref-tbles (push-end table (ref-tbles node)) :q (append (q node) join-query)))))

;;OK
(defun where-node (node attribute operator value att)
  "function that creates a node with the WHERE clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
          (condition (list :where (list operator (intern attribute) val)))
          (child (make-instance 'node :id (make-id) :parent node :depth (+ (depth node) 1) :q (append (q node) condition) :attrs att :tble (tble node))))
    child))

;;OK
(defun and-node (node attribute operator value)
  "function that creates a node with the AND clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
          (q (concatenate 'string (q node) " AND " (name attribute) " " operator " '" val "'"))
          (child (make-instance 'node :id (make-id) :parent node :depth (+ (depth node) 1) :q q :attrs (append (attrs node) (list attribute)) :tble (tble node))))
    child))

;;OK
(defun or-node (node attribute operator value)
  "function that creates a node with the OR clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
          (q (concatenate 'string (q node) " OR " (name attribute) " " operator " '" val "'"))
          (child (make-instance 'node :id (make-id) :parent node :depth (+ (depth node) 1) :q q :attrs (append (attrs node) (list attribute)) :tble (tble node))))
    child))