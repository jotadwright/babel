(in-package :database-querying)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for querying an sql database ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun query-database (question)
  "Comprehends the question, queryies the database and returns the answer."
  (let (fcg-meaning fcg-query formatted-query result)
    (notify fcg::query-started question)
    (setf fcg-meaning (comprehend question))
    (notify fcg::query-completion-started fcg-meaning)
    (setf sql-query (substitute-query-parts fcg-meaning))
    (setf formatted-query (format-sql-query sql-query))
    (notify fcg::query-completion-finished formatted-query)
    (setf result (execute-sql-query :query formatted-query))
    (notify fcg::query-finished result)
"See result in output browser or web interface if a monitor is activated."))
