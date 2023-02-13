
(ql:quickload :qc)          
        
;;init tree
(let* ((root (make-instance 'node :id 0 :query "" :parent-node nil :depth 0))
      (tree (make-instance 'query-tree :query "" :nodes (list root) :root-node root))
      (db (init-table)))
  (general db tree 't)
  (write (length (nodes tree))))


