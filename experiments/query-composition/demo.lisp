(ql:quickload :qc)
(in-package :qc)


;; setup create table and insert data into your database for corresponding to the experience
;; for this, replace the value of differents keys to yours connections options
;(setup-database :dbname "lisp_db"   :username "admin"   :password "root" :hostname "localhost")

;Connect database
(connect-toplevel "lisp_db" "admin" "root" "localhost")

;; Run a test
(let* ((master (make-instance 'master-agent))
        (question (make-instance 'question :question "Test" :query-associated "Select name from country where id=1")))
  (run question))

;Disconnect database
(disconnect-toplevel)