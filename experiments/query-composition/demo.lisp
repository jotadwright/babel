(ql:quickload :qc)
(in-package :qc)


;; setup create table and insert data into your database for corresponding to the experience
;; for this, replace the value of differents keys to yours connections options
;(setup-database :dbname "lisp_db"   :username "admin"   :password "root" :hostname "localhost")

;Connect database
(connect-toplevel "lisp_db" "admin" "root" "localhost")


(let ((composer-obj (make-instance 'query-composer)))
  (write (compose-query2 composer-obj (query "select name from country where id=1")
                         :sort-table t)))



;Test the research
(let* ((master (make-instance 'master-agent))
       (quest (get-question master))
       (composer-obj-1 (make-instance 'query-composer))
       (composer-obj-2 (make-instance 'query-composer))
       (composer-obj-3 (make-instance 'query-composer))
       (composer-obj-4 (make-instance 'query-composer))
       (start-time (get-internal-real-time))
       (node-found nil))
  (write (query-associated quest))
  (terpri)
  ;;All sort
 (setf node-found (compose-query2 composer-obj-1 (query (query-associated quest))
                         :exclude-constraint t
                         :sort-table t
                         :star-shortcut t))
  (make-html-report composer-obj-1 quest (- (get-internal-real-time) start-time) node-found '("Exclude constraint" "Sort table" "Star shortcut"))
  ;;Sort-table & shortcut-sort
  (setf start-time (get-internal-real-time))
  (setf node-found (compose-query2 composer-obj-2 (query (query-associated quest))
                         :sort-table t
                         :star-shortcut t))
  (make-html-report composer-obj-2 quest (- (get-internal-real-time) start-time) node-found '("Sort table" "Star shortcut"))
  ;;Only short-cut sort
  (setf start-time (get-internal-real-time))
  (setf node-found (compose-query2 composer-obj-3 (query (query-associated quest))
                         :star-shortcut t))
  (make-html-report composer-obj-3 quest (- (get-internal-real-time) start-time) node-found '("Star shortcut"))
  ;;None sort
  (setf start-time (get-internal-real-time))
  (setf node-found (compose-query2 composer-obj-4 (query (query-associated quest))))
  (make-html-report composer-obj-4 quest (- (get-internal-real-time) start-time) node-found '()))

;Disconnect database
(disconnect-toplevel)

(defun get-node-with-function (func selection)
  (let ((new-nodes '()))
    (loop for select in selection do
             (loop for att in select do
                      (if (equal (type-att att) 'integer)
                        )))))
                                          
(defun remplacer-element (liste element-a-remplacer nouvel-element)
  (setf (nthcdr (position element-a-remplacer liste))
        (cons nouvel-element (cdr (nthcdr (position element-a-remplacer liste)))))
  liste)