(in-package :qc)

(defclass node ()
  ((id :type integer
       :initarg :id
       :accessor id)
   (parent :type node
           :initarg :parent
           :initform nil
           :accessor parent)
   (children :type list
             :initarg :children
             :initform '()
             :accessor children)
   (depth :type integer
          :initarg :depth
          :initform nil
          :accessor depth
          :documentation "Depth of the node.")
   (tble :type table
         :initarg :tble
         :initform nil
         :accessor tble
         :documentation "Table referred in SELECT form.")
   (ref-tbles :type list
              :initarg :ref-tbles
              :initform '()
              :accessor ref-tbles
              :documentation "Set of tables referred to in the query.")
   (selection :type list
              :initarg :selection
              :initform '()
              :accessor selection
              :documentation "Selection part f the query used for children.")
   (conditions :type list
               :initarg :conditions
               :initform '()
               :accessor conditions
               :documentation "Condition part of the request used for children.")
   (attrs :type list
          :initarg :attrs
          :initform '()
          :accessor attrs)
   (time-result :type float
                :initarg :time-result
                :initform nil
                :accessor time-result
                :documentation "Execution time of the query.")
   (q :type integer
      :initarg :q
      :initform ""
      :accessor q
      :documentation "Query generated"))
  (:documentation "Object representing a node of the tree. This object is represented by a set of attributes that allows it to obtain and store all the information necessary for its SQL query."))

;;OK
(defun init-node (node attributes table &key join)
  "function that create a node with the SELECT .. FROM .. clause and return the newly created node with its associated parent."
    (let* ((q '(:select))
            (table-name (intern (string-upcase (name table)))))
      (dolist (att-n attributes)
          (if join
            (progn
              (let ((att (intern (string-upcase (concatenate 'string (symbol-name table-name) "." att-n)))))
                (setf q (push-end att q))))
            (progn
               (setf q (push-end (intern (string-upcase att-n)) q)))))
      (setf q (push-end :from q))
      (setf q (push-end table-name q))
      (make-instance 'node
                     :id (make-id)
                     :parent node
                     :depth (+ (depth node) 1)
                     :q q
                     :selection q
                     :tble table
                     :ref-tbles (list table))))

;;OK
(defun join-node (node ref-info table-obj &key foreign-ref outer-join)
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
  (make-instance 'node
                 :id (make-id)
                 :parent node
                 :depth (depth node)
                 :tble (tble node)
                 :ref-tbles (push-end table-obj (ref-tbles node))
                 :q (append (q node) join-query)
                 :selection (append (q node) join-query)))))

;;OK
(defun where-node (node attribute operator value att)
  "function that creates a node with the WHERE clause and returns the newly created node with its associated parent."
  (let* ((val-att (change-type value))
           (condition nil))
    (if (equal (length (ref-tbles node)) 1)
      (setf condition (list operator (intern (string-upcase attribute)) val-att))
      (setf condition (list operator (intern (string-upcase (concatenate 'string (name (tble node)) "." attribute))) val-att))) 
    (make-instance 'node
                               :id (make-id)
                               :parent node
                               :depth (+ (depth node) 1)
                               :q (append (q node) (list :where condition))
                               :attrs att
                               :tble (tble node)
                               :ref-tbles (ref-tbles node)
                               :selection (selection node)
                               :conditions (push-end condition (conditions node)))))

;;OK
(defun and-node (node attribute operator value)
  "function that creates a node with the AND clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
          (and-condition nil))
    (if (equal (length (ref-tbles node)) 1)
      (setf and-condition (list :and (conditions node) (list operator (intern (name attribute)) val)))
      (setf and-condition (list :and (conditions node) (list  operator (intern (concatenate 'string (name (tble node)) "." (name attribute))) val))))
    (make-instance 'node
                   :id (make-id)
                   :parent node
                   :depth (+ (depth node) 1)
                   :q (append (selection node) (list :where and-condition))
                   :attrs (push-end attribute (attrs node))
                   :tble (tble node)
                   :selection (selection node)
                   :conditions and-condition)))

;;OK
(defun or-node (node attribute operator value)
  "function that creates a node with the OR clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
          (or-condition nil))
    (if (equal (length (ref-tbles node)) 1)
      (setf or-condition (list :or (conditions node) (list operator (intern (name attribute)) val)))
      (setf or-condition (list :or (conditions node) (list  operator (intern (concatenate 'string (name (tble node)) "." (name attribute))) val))))
    (make-instance 'node
                   :id (make-id)
                   :parent node
                   :depth (+ (depth node) 1)
                   :q (append (selection node) (list :where or-condition))
                   :attrs (push-end attribute (attrs node))
                   :tble (tble node)
                   :selection (selection node)
                   :conditions or-condition)))