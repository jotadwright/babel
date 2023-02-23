(in-package :ont-alignment)

(defun generate-sql-query (table columns &optional where)
  "Generates a SQL query for selecting data from a table with optional WHERE clause."
  (let ((query (format nil "SELECT ~{~A~^, ~} FROM ~A" columns table)))
    (when where
      (setf query (format nil "~A WHERE ~A" query where)))
    query))

(defun transpose (list-of-lists)
  "Transposes a list of lists, returning a list of columns."
  (apply #'mapcar #'list list-of-lists))

(defun get-all-columns (table)
  "Fetches all columns from a given table and uses transpose function to make a list of them"
  (let* ((columns-list-of-lists (postmodern:query (:select :all :from 'information_schema.columns :where (string= "table_name" table)) :results-type :list))
         (columns-list (transpose columns-list-of-lists)))
    columns-list))

(defun try-queries-until-success (table answer)
  "Tries different queries to get to the answer without error.
   Returns the first successful query."
  (let ((columns-list (get-all-columns table)))
    (loop for column in columns-list
          do (handler-case
                (generate-sql-query table column "~d = '~d'" column answer)
               (error (condition) (format t "Error: ~a~%" condition))))))