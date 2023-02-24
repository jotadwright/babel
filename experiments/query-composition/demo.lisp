(ql:quickload :qc)
(in-package :qc)


;; setup create table and insert data into your database for corresponding to the experience
;; for this, replace the value of differents keys to yours connections options
(setup-database :dbname "lisp_db"
                           :username "admin"
                           :password "root"
                           :hostname "localhost")

;; test the function compose, this function return the first query
;; when u made the call of the method Compose she return the first query.
;; after, when u call a second time... etc they return also the first query meet until the queue is empty. (IN PROGRESS)

;; the test is perform on a basic query "SELECT .... FROM ...

(connect-toplevel "lisp_db" "admin" "root" "localhost")
(let* ((result (query "SELECT name FROM continent where name='Africa'"))
       (root-node (make-instance 'node :id 0 :parent nil :children '() :depth 0 :q ""))
       (tree (make-instance 'query-tree :nodes  (list  root-node) :root root-node))
       (composer-obj (make-instance 'query-composer :queue (nodes tree)  :tree tree :tables (init-schema))))
  (format t (compose-query composer-obj result :exclude-id t)))

(disconnect-toplevel)