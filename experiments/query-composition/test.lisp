(ql:quickload :qc)          
(in-package :qc)


(connect-toplevel "master_db" "postgres" "root" "localhost")

(let* ((result (query "select id from continent"))
       (node (make-instance 'node :id 0 :parent nil :children '() :depth '0 :q ""))
      (tree (make-instance 'query-tree :nodes '(node) :root node :q "")))
  (write result)
  (create (list node) tree 1 result (init-schema)))

(disconnect-toplevel)