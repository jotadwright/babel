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
(let ((start-time (get-internal-real-time))
       (result (query "SELECT name, population from country where id=1"))
       (composer-obj (make-instance 'query-composer))
       (start-time (get-internal-real-time)))

  (write (compose-query2 composer-obj result
                         :exclude-constraint t
                         :sort-table t
                         :star-shortcut t))
  (terpri)
  (write (- (get-internal-real-time) start-time))
  (terpri))


;; show all the queries
(let ((result (query "SELECT name FROM city WHERE id=1"))
       (composer-obj (make-instance 'query-composer)))
  (write (compose-query composer-obj result :exclude-id t :all-queries t)))

(disconnect-toplevel)

;don't use right now
(let* ((master (make-instance 'master-agent))
       (quest (q (get-question master)))
       (composer-obj (make-instance 'query-composer)))
  (write quest)
  (terpri)
  (write (compose-query composer-obj (query quest) :exclude-id t))
  (terpri))


(add-element (make-html-for-t (make-instance 'query-composer)))


(defmethod make-html-for-t ((tree query-tree) ts)
  `((h1), (root tree)))

(defmethod make-html-for-search-tree ((tree query-tree))
  (
  (write "test"))
  
                    