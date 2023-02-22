(in-package :ont-alignment)

(defun connecting-to-db (agent db-name db)
  "Allows to connect to a database: can be used multiple times if connecting to more than one database."
  (let ((db-connection (postmodern::connect-toplevel db "postgres" "postgres" "localhost")))
    (setf (personal-db agent) db-name) ;check for a way for it not to be reinitialized
    (format t "The name of the database is ~d" (personal-db agent))
    (format t "The connection is established between ~d and the database." agent)))