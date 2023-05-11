(in-package :ont-alignment)

;---------------;
;Query functions;
;---------------;

(defun extract-answer-columns (column-names-list answer)
  "From a given answer, retrieves the possible columns the answer belongs to.
  This functions is only used when the answer is of datatype varchar, or list. "
  (let* ((table_name (first (car column-names-list)))
         (possible-answer-columns '()))
    (push
     (loop for item in column-names-list
           for columns-list = (cdr item)
           append (if (consp answer)
                    (loop for column in columns-list
                          for column_name = (first column)
                          for result = (ignore-errors (execute-postmodern-query (generate-query-string table_name :column column_name :where column_name :in T :set answer)))
                          when result collect column)
                    (loop for column in columns-list
                          for column_name = (first column)
                          for result = (ignore-errors (execute-postmodern-query (generate-query-string table_name :column column_name :where column_name :eq answer)))
                          when result collect column)))
      possible-answer-columns)
    possible-answer-columns))

(defun generate-query-list (table &key (column "*") (count nil) (where nil) (eq nil) (in nil) (set nil))
  "Allows an agent to generate a basic sql query string. The table name is mandatory.
  The optional arguments are :
  - COLUMN : the name of the column to select (defaults to '*' to select all columns).
  - COUNT : a boolean indicating whether to count the number of rows. If nil, returns all rows.
  - WHERE : column to test.
  - IN : a subquery string"
  (let* ((select-clause (if count (list :select :count column) (list :select column)))
        (from-clause (list :from table))
        (eq-clause (if eq (list :where (list := where eq)) ""))
        (in-clause (if in (list :where (:in where (:set in))) ""))
        (query-string (concatenate 'list select-clause from-clause eq-clause in-clause)))
    query-string))

(defun execute-postmodern-query (query-list)
  "Generates a query that postmodern library can process."
  (let* ((compiled-query (sql-compile query-list))
        (result (query compiled-query)))
    (print compiled-query)
    result))

;(execute-postmodern-query (generate-query-list 'actorsfilms :column 'film :where 'actor :eq "Gerard Depardieu"))

;(defvar *z* '(:select 'film :from 'actorsfilms :where (:= 'film "Perfect Combination")))
;(query (sql-compile *z*))

;(query "select film from actorsfilms where film = any($1)" #("Toxin" "Black Coffee" "Perfect Combination")) 

;(query (:select 'film :from 'actorsfilms :where (:in 'film (:set "Toxin" "Black Coffee" "Perfect Combination"))))
;(query "SELECT film FROM actorsfilms WHERE film IN ('Toxin', 'Black Coffee', 'Perfect Combination')")

;(execute-postmodern-query (generate-query-string 'actorsfilms :column 'film :where 'film :in T :set "Toxin" "Black Coffee" "Perfect Combination"))

;----------------------------;
;Initializing the search tree;
;----------------------------;

(defclass node (tree-node)
  ((parent
    :accessor parent
    :initarg :parent
    :initform nil
    :type tree-node
    :documentation "The previous node of a given node in the tree")
   (children
    :accessor children
    :initarg :children
    :initform '()
    :type list
    :documentation "A list of all the tree nodes a node can have.")
   (query-list
    :accessor query-list
    :initarg :query-list
    :initform '()
    :type list
    :documentation "A query in the form of a list that can be compiled.")
   (sql-keywords
    :accessor sql-keywords
    :initarg :sql-keywords
    :initform nil
    :type list
    :documentation "A list of the sql keywords that were used for the query")
   (tble
    :type table
    :initarg :tble
    :initform nil
    :accessor tble
    :documentation "Table referred in SELECT form.")
   (attributes-selected
    :type list
    :initarg :attributes-selected
    :initform '()
    :accessor attributes-selected
    :documentation "Attribute used for Selection part of the query")
   (ref-tbles
    :type list
    :initarg :ref-tbles
    :initform '()
    :accessor ref-tbles 
    :documentation "Set of tables referred to in the query.")
   (selection
    :type list
    :initarg :selection
    :initform '()
    :accessor selection
    :documentation "Selection part of the query used for children.")
   (conditions
    :type list
    :initarg :conditions
    :initform '()
    :accessor conditions
    :documentation "Condition part of the request used for children.")
   (attrs
    :type list
    :initarg :attrs
    :initform '()
    :accessor attrs
    :documentation "attributes reffered in condition form.")
   (depth
    :accessor depth
    :initarg :depth
    :initform nil
    :type integer
    :documentation "Depth of the node in the tree.")))
  
(defun make-root-node ()
   "Creates a new tree with its initial node."
   (let ((root-node (make-instance 'node
                                   :depth 0)))
     root-node))

;-------------;
;BFS functions;
;-------------;

(defun first-expand (answer)
  "Create the first expansion of the tree. This will take a SELECT ... FROM table form."
  ;we check if the answer is constituted of multiple values ; if not the select is simple
  ;it happens that the answer is a list of lists
  (let* ((column-names-list (get-all-tables))
         (selected-columns
          (cond
           ((consp answer)
            (extract-answer-columns column-names-list answer))
           ((stringp answer)
            (extract-answer-columns column-names-list answer))
           ((integerp answer)
            '(1 2 3))))
      ;test first columns with type integer
      ;if no result, then use an aggregate function
         (possible-queries (loop for column in selected-columns
                                 for possible-query = (generate-query-list (first column) :column (second column))
                                 collect possible-query)))
;test int columns or use count keyword with all the columns
      ;if answer is part of a foreign-key column, we can assume we need to use another table of the database. 
    possible-queries))


(defun expand ()
  "Create any expansion, except for the first one."
  (print "ok")
  )

(defun bfs (initial-node goal-test)
  "An implementation of bfs algorithm using a list as queue."
  (let ((queue (list initial-node))
        (explored '())
        (result 0))
    (first-expand goal-test)
    (loop until (not (null result)) do
            (let* ((node (pop queue)) ;;we choose the shallowest node in the queue
                   (node-query-result (execute-postmodern-query (query-list node)))
                   (equality-test (equalp goal-test node-query-result)))
              (if equality-test
                (progn
                  (format t "Success")
                  (print "ok")
                  (setf result 1))
                (push node explored))))
    (when (null queue)
      (loop for node in explored
            do (expand node)))))

;(bfs (make-root-node) '("The Neon Demon" "The Dark Knight Rises" "TiMER" "Not Since You" "Life Is Hot in Cracktown" "Exit Speed" "Stories USA" "Three Way" "Wrong Turn" "Love Object" "We Were Soldiers" "Ghost Ship" "The Hole" "Riding in Cars with Boys" "My First Mister" "Boiler Room" "The Messenger: The Story of Joan of Arc"))
