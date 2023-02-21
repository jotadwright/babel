(ql:quickload :qc)
(in-package :qc)

;; DBNAME USER PASSWORD HOST
(connect-toplevel "master_db" "postgres" "root" "localhost")

(write (query "select id from continent where name='Africa'"))


(let* ((result (query "select id from continent"))
       (node (make-instance 'node :id 0 :parent nil :children '() :depth '0 :q ""))
      (tree (make-instance 'query-tree :nodes '(node) :root node :q "")))
  (write result)
  (query-generator (list node) tree result (init-schema)))

(disconnect-toplevel)





(sql (:select 'size :from 'continent))

(query (:select 'size :from 'continent))

(let* ((result (query (:select 'size :from 'continent)))
      (val (change-type (first (first result)))))
  (write (type-of val))
  (query (concatenate 'string "SELECT id FROM continent WHERE size= " val)))

(let* ((result (query (:select 'name :from 'continent)))
       (val (change-type (first (first result))))
       (q (concatenate 'string "SELECT id FROM continent WHERE name= \'"val"\'")))
  (write (type-of (first (first result))))
  (write q)
  (query q))
  