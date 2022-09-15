(in-package :database-querying)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An interface between FCG and MySQL databases ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exports
(export '(execute-sql-query print-query-result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting some parameters                              ;;
;; (set up a database on your localhost for the moment) ;;
;; Later we can deploy one on fcg-net.org               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sql-path* "/usr/local/mysql/bin/mysql")
(defparameter *sql-username* "fcg-user")
(defparameter *sql-host* "localhost")
(defparameter *sql-password* "FCGfcg2022!")

;;;;;;;;;;;;;;;;;;;;;;;
;; Executing a query ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-sql-query (&key query
                               (return-header nil)
                               (database "fcg-data")
                               (sql-path *sql-path*)
                               (username *sql-username*)
                               (host *sql-host*)
                               (password *sql-password*))
  "Executes an SQL query and returns the result (list of lists)"
  ;; Check Input
  (unless query (error "Query should not be nil!"))
  ;; Construct and execute query
  (let* ((username (string-append "-u " username))
         (password (string-append "--password=" password))
         (return-header (if return-header "" "-N"))
         (query (string-append "-e " query))
         (host (string-append "-h " host))
         (result (exec-and-return sql-path username host password return-header query database)))
    ;; Check result and delete warning
    (setf result (delete-if #'(lambda (line)
                                (and (> (length line) 17)
                                     (equalp "mysql: [Warning]" (subseq line 0 16)))) result))
    ;; Split on tab
    (mapcar #'(lambda (line)
                (split-sequence:split-sequence #\TAB line)) result)))

(defun print-query-result (query-result &key (stream t))
  "Prints a query result in a nicer format."
  (format stream "~{~{~a    |    ~}~%~}" query-result))


;;;;;;;;;;;;;
;; Example ;;
;;;;;;;;;;;;;

;; (setf *result*  (execute-sql-query :query "'SELECT lib_mrq_utac, dscom, hybride, puiss_max FROM cars WHERE hybride=\"oui\" AND lib_mrq_utac=\"BMW\"'" :return-header t))
;; (print-query-result *a*)
   