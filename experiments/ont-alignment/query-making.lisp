(in-package :ont-alignment)

(defun generate-query-string (table &key (column "*") (count nil) (where nil) (in nil))
  "Allows an agent to generate a basic sql query string. The table name is mandatory.
  The optional arguments are :
  - COLUMN : the name of the column to select (defaults to '*' to select all columns).
  - COUNT : a boolean indicating whether to count the number of rows. If nil, returns all rows.
  - WHERE : a string representing the WHERE clause ; can be column_name=value, or a column_name if IN clause.
  - IN : a subquery string"
  (let ((select-clause (if count (format nil "SELECT COUNT(~a)" column) (format nil "SELECT ~a" column)))
        (from-clause  (format nil "FROM ~a"table))
        (where-clause (if where (format nil "WHERE ~a" where) ""))
        (in-clause (if in (format nil "IN (~a)" in) "")))
    (concatenate 'string select-clause " " from-clause " " where-clause " " in-clause)))

;example (we can generate as many IN-clause as we want):
;(generate-query-string films :column "film_name" :where "film_id" :in (generate-query-string "actors_films_relations" :column film_id :where "actor_id" :in (generate-query-string "actors" :column "actor_id" :where (concatenate "actor =" "'Gerard Depardieu'"))))
;(generate-postmodern-query (basic-sql-grammar "actorsfilms" :column "film" :where (concatenate 'string "actor" " = " "'Gerard Depardieu'")))

(defun generate-postmodern-query (query-string)
  "Generates a query that postmodern library can process."
  (let ((result (postmodern::query query-string)))
    result))

(defun is-substring (substring string)
  "Check if a substring is part of a string."
  (let ((result (search substring string)))
    (if (not (null result))
      (format t "~a is a substring of ~a~%" substring string)
      (format t "~a is not a substring of ~a~%" substring string))))

(defun query-reverse-engineering (column-names-list answer)
  "From a given answer, tries to synthesize the query that led to it."
  ;step 1 : we need to get the column and table where the answer was found
  (let* ((answer-column (loop for item in column-names-list
                           append (loop for column in (first (cdr item))
                                        for result = (ignore-errors (generate-postmodern-query (format nil "SELECT ~a FROM ~a WHERE ~a = '~a'" column (first (car column-names-list)) column answer))) 
                                        when result return (list column))))
  ;step 2 : we need to find out from which column we need to start the query to get to the answer
  ;SELECT col FROM actorsfilms WHERE select-column = answer
         (where-column (loop for item in updated-col-names-list
                             append (loop for column in (first (cdr item))
                                          for result = (generate-postmodern-query (format nil "SELECT ~a FROM ~a WHERE ~a = '~a'" column (first (car column-names-list)) answer-column answer))
                                          when result return result))))
    where-column))
    
(defun try-queries-until-success (answer)
  "Tries different queries to get to the answer without error.
   Returns the first successful query."
  (let ((tables-list (postmodern::list-tables t))
        ;the list-tables function retrieves all the tables of a given database and if the strings-p arg is set to true the result is list of strings
        (column-names-list '()))
        ;this variable is aimed to store an association list of each table and its columns
    (format t "The different tables of the database are: ~a~%" tables-list)
    (loop for table in tables-list
          do (progn (push (cons table (list (postmodern::list-columns table))) column-names-list)
                (format t "The different columns of the table ~a are: ~a~%" table (postmodern:list-columns table))))
    (print (query-reverse-engineering column-names-list answer))))

;(postmodern::query "SELECT film FROM actorsfilms WHERE actor = 'Gerard Depardieu'")

;(postmodern::query "SELECT count(film) FROM actorsfilms WHERE actor = 'Gerard Depardieu'")