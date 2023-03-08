(ql:quickload :qc)
(in-package :qc)


;; setup create table and insert data into your database for corresponding to the experience
;; for this, replace the value of differents keys to yours connections options
(setup-database :dbname "lisp_db"
                           :username "admin"
                           :password "root"
                           :hostname "localhost")

(connect-toplevel "lisp_db" "admin" "root" "localhost")

;; show query one by one
(let ((result (query "SELECT name FROM continent WHERE name='Africa'"))
       (composer-obj (make-instance 'query-composer)))
  (format t (compose-query composer-obj result :exclude-id t))
  (terpri)
  (format t (compose-query composer-obj result :exclude-id t)))

;; show all the queries
(let ((result (query "SELECT name FROM continent WHERE name='Africa'"))
       (composer-obj (make-instance 'query-composer)))
  (format t (compose-query composer-obj result :exclude-id t :all-queries t)))

(disconnect-toplevel)
