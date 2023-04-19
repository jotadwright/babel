(in-package :ont-alignment)

(defun connecting-to-db (agent db-name db)
  "Allows to connect to a database: can be used multiple times if connecting to more than one database."
  (let ((db-connection (postmodern::connect-toplevel db "postgres" "postgres" "localhost")))
    (setf (personal-db agent) db-name) ;check for a way for it not to be reinitialized
    (format t "The name of the database is ~d~%" (personal-db agent))
    (format t "The connection is established between ~d and the database.~%" agent)))

(defclass db-representation ()
  
  )

(defun get-all-tables ()
  "Retrieves a list of sublists. In each sublist : a table of the db, its columns with their data types."
  (let ((tables-list (list-tables t))
        ;list-tables function retrieves all tables of a given database
        ;if the strings-p arg is set to true the result is a list of strings
        (column-names-list '()))
        ;this variable is aimed to store an association list of each table and its columns
    (format t "The different tables of the database are: ~a~%" tables-list)
    (loop for table in tables-list
          do (progn (push (cons table (list (list-columns-with-types table))) column-names-list)
                (format t "The different columns of the table ~a are: ~a~%" table (list-columns table))))
    (print column-names-list)
    column-names-list))

;(get-all-tables)
;'(("actorsfilms" (("actor" "varchar") ("actor_id" "varchar") ("film" "varchar") ("year" "int4") ("votes" "int4") ("rating" "float8") ("film_id" "varchar"))))

(defvar tables (get-all-tables))
(loop for i in tables
      for columns-list = (cdr i)
        append (loop for column in columns-list
                       do (print column)))
