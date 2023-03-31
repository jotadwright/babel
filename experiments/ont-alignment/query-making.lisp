(in-package :ont-alignment)

;---------------;
;query functions;
;---------------;

(defun get-all-tables ()
  "Retrieves a list of sublists. In each sublist : a table of the db, its columns with their data types."
  (let ((tables-list (postmodern:list-tables t))
        ;list-tables function retrieves all tables of a given database
        ;if the strings-p arg is set to true the result is a list of strings
        (column-names-list '()))
        ;this variable is aimed to store an association list of each table and its columns
    (format t "The different tables of the database are: ~a~%" tables-list)
    (loop for table in tables-list
          do (progn (push (cons table (list (postmodern:list-columns-with-types table))) column-names-list)
                (format t "The different columns of the table ~a are: ~a~%" table (postmodern:list-columns table))))
    column-names-list))

;(get-all-tables)
;(("actorsfilms" (("actor" "varchar") ("actor_id" "varchar") ("film" "varchar") ("year" "int4") ("votes" "int4") ("rating" "float8") ("film_id" "varchar"))))

(defun extract-answer-columns (column-names-list answer)
  "From a given answer, retrieves the possible columns (and columns) the answer belongs to.
  This functions is only used when the answer is of datatype varchar, or list. "
  (let* ((table_name (first (car column-names-list)))
         (possible-answer-columns '()))
    (push
     (loop for item in column-names-list
           for columns-list = (first (cdr item))
           append (if (consp answer)
                    (loop for column in columns-list
                          for result = (ignore-errors (generate-postmodern-query (generate-query-string table_name :column column :where column :in T :set answer)))
                          when result collect column)
                    (loop for column in columns-list
                          for result = (ignore-errors (generate-postmodern-query (generate-query-string table_name :column column :where column :eq answer)))
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

(defun generate-postmodern-query (query-list)
  "Generates a query that postmodern library can process."
  (let* ((compiled-query (postmodern:sql-compile query-list))
        (result (postmodern:query compiled-query)))
    (print compiled-query)
    result))

;(generate-postmodern-query (generate-query-list 'actorsfilms :column 'actor :where 'actor :eq "Gerard Depardieu"))

;(defvar *z* '(:select 'film :from 'actorsfilms :where (:= 'film "Perfect Combination")))
;(postmodern:query (postmodern:sql-compile *z*))

;(query "select film from actorsfilms where film = any($1)" #("Toxin" "Black Coffee" "Perfect Combination")) 

;(postmodern:query (:select 'film :from 'actorsfilms :where (:in 'film (:set "Toxin" "Black Coffee" "Perfect Combination"))))
;(postmodern:query "SELECT film FROM actorsfilms WHERE film IN ('Toxin', 'Black Coffee', 'Perfect Combination')")

;(generate-postmodern-query (generate-query-string 'actorsfilms :column 'film :where 'film :in T :set "Toxin" "Black Coffee" "Perfect Combination"))

;----------------------------;
;Initializing the search tree;
;----------------------------;

(defclass tree-node ()
  ((parent
    :accessor parent
    :initarg :parent
    :initform nil
    :type tree-node
    :documentation "The previous node of a given node in the tree")
   (children
    :accessor children
    :initarg :children
    :initform nil
    :type list
    :documentation "A list of all the tree nodes a node can have.")
   (query-list
    :accessor query-list
    :initarg :query-list
    :initform nil
    :type list
    :documentation "A query in the form of a list that can be compiled.")
   (sql-keywords
    :accessor sql-keywords
    :initarg :sql-keywords
    :initform nil
    :type list
    :documentation "A list of the sql keywords that were used for the query")
   (expand-nb
    :accessor expand-nb
    :initarg :expand-nb
    :initform nil
    :type int
    :documentation "An informative slot on which number of expand the node is part of.")))
  
(defun make-tree ()
   "Creates a new tree with its initial node."
   (make-instance 'tree-node
                  :expand-nb 0))  

;-------------;
;BFS functions;
;-------------;

;(ql:quickload "queues")
;(ql:quickload "queues.simple-queue")

(defun bfs (graph start)
  "An implementation of bfs algorithm using a queue to keep track of the nodes to visit next,
  and a hash table to keep track of the visited nodes. The graph argument should be a hash table
  where each key is a node in the graph, and the corresponding value is a list of its neighbors."
  (let ((queue (queues:make-queue :simple-queue))
        (visited (make-hash-table)))
    (queues:qpush queue "")
    (setf (gethash start visited) t)
    (loop while queue do
          (let ((node (queues:qpop queue)))
            (print node)
            (dolist (neighbor (gethash node graph))
              (unless (gethash neighbor visited)
                (setf (gethash neighbor visited) t)
                (queues:qpush queue neighbor)))))))

;-----;
;other;
;-----;

(defun try-queries-until-success (answer)
  "Tries different queries to get to the answer without error.
   Returns the first successful query."
  (let ((tables-list (postmodern:list-tables t))
        ;the list-tables function


        retrieves all the tables of a given database and if the strings-p arg is set to true the result is list of strings
        (column-names-list '()))
        ;this variable is aimed to store an association list of each table and its columns
    (format t "The different tables of the database are: ~a~%" tables-list)
    (loop for table in tables-list
          do (progn (push (cons table (list (postmodern:list-columns table))) column-names-list)
                (format t "The different columns of the table ~a are: ~a~%" table (postmodern:list-columns table))))
    (print (extract-answer-columns column-names-list answer))))

