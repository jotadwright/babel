(ql:quickload :qc)
(in-package :qc)

;; DBNAME USER PASSWORD HOST
(connect-toplevel "master_db" "postgres" "root" "localhost")

(let* ((result (query "select name from city where name='city0'"))
       (node (make-instance 'node :id 0 :parent nil :children '() :depth '0 :q ""))
      (tree (make-instance 'query-tree :nodes '(node) :root node :q "")))
  (query-generator (list node) tree result (init-schema) :exclude-id t))

(disconnect-toplevel)

(setup-database)


