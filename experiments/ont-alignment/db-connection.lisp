(in-package :ont-alignment)

(defun connecting-to-db (agent-id db_name db)
  "Allows to connect to a database : can be use mutliple times if connecting to more than one database."
  (setf db_name (postmodern::connect-toplevel db "postgres" "postgres" "localhost"))
  (format t "The connection is established between ~d and the following database : ~d." agent-id db_name)
)