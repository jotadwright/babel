(in-package :ont-alignment)

(defun generate-sql-query (table column where)
  "Generates a SQL query for selecting data from a table with optional WHERE clause."
  (let* ((sql-query (format nil "SELECT ~a FROM ~a WHERE ~a" column table where))
         (result (postmodern::query sql-query)))
    (print result)))

(defun get-all-entities (table)
  "Fetches all entities from a given table and returns them in a list.
   The result is, as a consequence, a list of lists."
  (postmodern::query "SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = $1" table))

(defun get-nth-element (lst n)
  "Returns the nth element of list lst"
  (nth n lst))

(defun try-queries-until-success (table answer)
  "Tries different queries to get to the answer without error.
   Returns the first successful query."
  (let* ((entity-list (get-all-entities table))
         (column-name-list (loop for entity in entity-list
                                 collect (get-nth-element entity 3))))
    (format t "The different columns of the table are: ~a~%" column-name-list)
    (loop for column in column-name-list
          for result = (handler-case (generate-sql-query table column (format nil "~a = '~a'" column answer))
                          (condition () nil))
          when result return (get-nth-element (get-nth-element result 0) 0))))