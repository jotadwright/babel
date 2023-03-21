(in-package :qc)

(defclass node (tree-node)
  ((depth :type integer
          :initarg :depth
          :initform nil
          :accessor depth
          :documentation "Depth of the node.")
   (tble :type table
         :initarg :tble
         :initform nil
         :accessor tble
         :documentation "Table referred in SELECT form.")
   (attributes-selected :type list
                        :initarg :attributes-selected
                        :initform '()
                        :accessor attributes-selected
                        :documentation "Attribute used for Selection part of the query")
   (ref-tbles :type list
              :initarg :ref-tbles
              :initform '()
              :accessor ref-tbles 
              :documentation "Set of tables referred to in the query.")
   (selection :type list
              :initarg :selection
              :initform '()
              :accessor selection
              :documentation "Selection part of the query used for children.")
   (conditions :type list
               :initarg :conditions
               :initform '()
               :accessor conditions
               :documentation "Condition part of the request used for children.")
   (attrs :type list
          :initarg :attrs
          :initform '()
          :accessor attrs
          :documentation "attributes reffered in condition form.")
   (time-result :type float
                :initarg :time-result
                :initform nil
                :accessor time-result
                :documentation "Execution time of the query.")
   (q :type list
      :initarg :q
      :initform nil
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
                     :attributes-selected attributes
                     :tble table
                     :ref-tbles (list table))))

;;OK
;;REFACT
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
    (let* ((foreign-t (intern (string-upcase (concatenate 'string f-table "." f-column))))
            (main-t (intern (string-upcase (concatenate 'string table "." column))))
           (join-query (list join (intern (string-upcase f-table)) :on (list := foreign-t main-t))))
  (make-instance 'node
                 :id (make-id)
                 :parent node
                 :depth (depth node)
                 :tble (tble node)
                 :ref-tbles (push-end table-obj (ref-tbles node))
                 :q (append (q node) join-query)
                 :selection (append (q node) join-query)))))

;;OK
(defun where-node (node ref-table attribute operator value)
  "function that creates a node with the WHERE clause and returns the newly created node with its associated parent."
  (let* ((val-att (change-type value))
           (condition nil))
    (if (equal (length (ref-tbles node)) 1)
      (setf condition (list operator (intern (string-upcase (name attribute))) val-att))
      (setf condition (list operator (intern (string-upcase (concatenate 'string (name ref-table) "." (name attribute)))) val-att))) 
    (make-instance 'node
                               :id (make-id)
                               :parent node
                               :depth (+ (depth node) 1)
                               :q (append (q node) (list :where condition))
                               :attrs (list attribute)
                               :tble (tble node)
                               :ref-tbles (ref-tbles node)
                               :selection (selection node)
                               :conditions condition)))

;;OK
(defmethod and-node ((composer query-composer) node ref-table attribute operator value)
  "function that creates a node with the AND clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
          (and-condition nil)
          (cdn nil)
          (permutation-query nil))
    (if (equal (length (ref-tbles node)) 1)
      (setf cdn (list operator (intern (string-upcase (name attribute))) val))
      (setf cdn (list  operator (intern (string-upcase (concatenate 'string (name ref-table) "." (name attribute)))) val)))
    (setf and-condition (list :and (conditions node) cdn))
    ;Set the permutation query
    (setf permutation-query (append (selection node) (list :where (list :and cdn (conditions node)))))
    ;Testing the query
    (if (not (node-test composer  permutation-query))
      (make-instance 'node
                     :id (make-id)
                     :parent node
                     :depth (+ (depth node) 1)
                     :q (append (selection node) (list :where and-condition))
                     :attrs (push-end attribute (attrs node))
                     :tble (tble node)
                     :selection (selection node)
                     :conditions and-condition))))

;;OK
(defmethod or-node ((composer query-composer) node ref-table attribute operator value)
  "function that creates a node with the OR clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
          (or-condition nil)
          (cbn nil)
          (permutation-query nil))
    (if (equal (length (ref-tbles node)) 1)
      (setf cdn (list operator (intern (string-upcase (name attribute))) val))
      (setf cdn (list  operator (intern (string-upcase (concatenate 'string (name ref-table) "." (name attribute)))) val)))
    (setf or-condition (list :or (conditions node) cdn))
    ;Set the permutation query
    (setf permutation-query (append (selection node) (list :where (list :or cdn (conditions node)))))
    ;Testing the query
    (if (not (node-test composer permutation-query))
      (make-instance 'node
                     :id (make-id)
                     :parent node
                     :depth (+ (depth node) 1)
                     :q (append (selection node) (list :where or-condition))
                     :attrs (push-end attribute (attrs node))
                     :tble (tble node)
                     :selection (selection node)
                     :conditions or-condition)
      nil)))


(defmethod node-test ((composer query-composer) q)
    (mapcar #'(lambda (x)
                (if (equal q (q x))
                  (return-from node-test t)))
              (queue composer)))

(defun length-query (node)
  (length (q node)))

;;Take too much time
;(defmethod node-test ((composer query-composer) depth q)
;  (let* ((all-nodes-with-depth (find-all depth (queue composer) :key #'depth :test #'=))
;          (all-nodes-with-length-q (find-all (length q) all-nodes-with-depth :key #'length-query :test #'=)))
;    (loop for queue-node in all-nodes-with-length-q
;             when (equal (q queue-node) q)
;             return nil)))
              