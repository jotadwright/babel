(in-package :ont-alignment)

(defun generate-query-string (table &key (column "*") (count nil) (where nil) (eq nil) (in nil) (set nil))
  "Allows an agent to generate a basic sql query string. The table name is mandatory.
  The optional arguments are :
  - COLUMN : the name of the column to select (defaults to '*' to select all columns).
  - COUNT : a boolean indicating whether to count the number of rows. If nil, returns all rows.
  - WHERE : a string representing the WHERE clause ; can be column_name=value, or a column_name if IN clause.
  - IN : a subquery string"
  (let* ((select-clause (if count (format nil ":select :count ~a" column) (format nil ":select ~a" column)))
        (from-clause (format nil ":from ~a" table))
        (where-clause (if where (format nil ":where ~a" where) ""))
        (eq-clause (if eq (format nil ":= where ~a" eq) ""))
        (in-clause (if in (format nil ":in  ~a" where) ""))
        (set-clause (if set (format nil "(:set ~a)" set) ""))
        (query-string (concatenate 'string select-clause " " from-clause " "  where-clause " " in-clause " " set-clause)))
    (print query-string)
    query-string))

(defun generate-postmodern-query (query-string)
  "Generates a query that postmodern library can process."
  (let ((result (postmodern:query (postmodern:sql-compile query-string))))
    result))

(defun extract-answer-columns (column-names-list answer)
  "From a given answer, retrieves the possible columns (and columns) where it was originally found."
  (let* ((table_name (first (car column-names-list)))
         (possible-answer-columns '()))
    ;step 1: finding out which column the answer belongs to
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
    ;step 2 : we need to find out from which column we need to start the query to get to the answer
    possible-answer-columns))

;(defvar *z* '(:select 'film :from 'actorsfilms :where (:= 'film "Perfect Combination")))
;(postmodern:query (postmodern:sql-compile *z*))

;(postmodern:query "select film from actorsfilms where film = any($1)" #("Toxin" "Black Coffee" "Perfect Combination")) 

;(postmodern:query (:select 'film :from 'actorsfilms :where (:in 'film (:set "Toxin" "Black Coffee" "Perfect Combination"))))
;(postmodern:query "SELECT film FROM actorsfilms WHERE film IN ('Toxin', 'Black Coffee', 'Perfect Combination')")

(generate-postmodern-query (generate-query-string 'actorsfilms :column 'film :where 'film :in T :set "Toxin" "Black Coffee" "Perfect Combination"))

(defun try-queries-until-success (answer)
  "Tries different queries to get to the answer without error.
   Returns the first successful query."
  (let ((tables-list (postmodern:list-tables t))
        ;the list-tables function retrieves all the tables of a given database and if the strings-p arg is set to true the result is list of strings
        (column-names-list '()))
        ;this variable is aimed to store an association list of each table and its columns
    (format t "The different tables of the database are: ~a~%" tables-list)
    (loop for table in tables-list
          do (progn (push (cons table (list (postmodern:list-columns table))) column-names-list)
                (format t "The different columns of the table ~a are: ~a~%" table (postmodern:list-columns table))))
    (print (extract-answer-columns column-names-list answer))))

