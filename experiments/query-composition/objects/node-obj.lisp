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
;;changer pour l'ajout des fonctions
;change bye using name attribute
(defun init-node (node attributes table &key join)
  "function that create a node with the SELECT .. FROM .. clause and return the newly created node with its associated parent."
    (let* ((q '(:select))
            (table-name (intern (string-upcase (name table)))))
      (dolist (att-n attributes)
          (if join
            (progn
              (let ((att (intern (string-upcase (concatenate 'string (symbol-name table-name) "." (name (first att-n)))))))
                (if (equal (length att-n) 2)
                  (setf q (push-end (list (second att-n) att) q))
                  (setf q (push-end att q)))))
            (progn
              (let ((att (intern (string-upcase (name (first att-n))))))
                (if (equal (length att-n) 2)
                  (setf q (push-end (list (second att-n) att) q))
                  (setf q (push-end att q)))))))
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
(defun join-node (node ref-info table-obj &key foreign-ref)
  "function that create a node with the ... INNER JOIN || OUTER JOIN ... ON ... =  ... clause and return the newly created node."
  (let* ((join :inner-join))
    (let* ((foreign-t (intern (string-upcase (concatenate 'string (foreign-table ref-info) "." (foreign-column ref-info)))))
            (main-t (intern (string-upcase (concatenate 'string (table-name ref-info) "." (column-name ref-info)))))
           (join-query (list join (intern (string-upcase (foreign-table ref-info))) :on (list := foreign-t main-t))))
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

              