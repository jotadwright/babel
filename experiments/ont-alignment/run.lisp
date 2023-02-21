;--the below process (before quickloading the ont-alignment package) is to be changed --;
;--but for the moment using it before quicloading is necessary because of a variable ---;
;--name conflict------------------------------------------------------------------------;

(use-package :SQL)
(require "sqlite")
(initialize-database-type :database-type :sqlite)

(defun connecting-to-db (agent-id db_name db)
  "Allows to connect to a database : can be use mutliple times if connecting to more than one database."
  (setf db_name (connect db))
  (format nil "The connection is established between ~d and the following database : ~d." agent-id db_name)
)

;example of application
(connecting-to-db "agent-1" "db2" "experiments/ont-alignment/dbs/db2_actors_films_simple_table.db")

;----------------------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------;

(ql:quickload :ont-alignment)
(in-package :ont-alignment)

(defparameter *experiment*
  (make-instance 'ont-alignment-experiment))

(run-interaction *experiment*)
