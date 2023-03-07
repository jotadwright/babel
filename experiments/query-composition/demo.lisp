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
(let* ((result (query "SELECT name FROM continent where name='Africa'"))
       (root-node (make-instance 'node :id 0 :parent nil :children '() :depth 0 :q ""))
       (tree (make-instance 'query-tree :nodes  (list  root-node) :root root-node))
       (composer-obj (make-instance 'query-composer :queue (nodes tree)  :tree tree :tables (init-schema))))
  (format t (compose-query composer-obj result :exclude-id t ))
  (terpri)
  (format t (compose-query composer-obj result :exclude-id t )))

;; show all the queries
(let* ((result (query "SELECT name FROM continent where name='Africa'"))
       (root-node (make-instance 'node :id 0 :parent nil :children '() :depth 0 :q ""))
       (tree (make-instance 'query-tree :nodes  (list  root-node) :root root-node))
       (composer-obj (make-instance 'query-composer :queue (nodes tree)  :tree tree :tables (init-schema))))
  (format t (compose-query composer-obj result :exclude-id t :all-queries t )))

(disconnect-toplevel)
