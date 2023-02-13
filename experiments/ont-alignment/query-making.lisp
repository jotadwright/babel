(in-package :ont-alignment)

(defun generate-query (db)
  (query "SELECT * FROM films" :database db) ;we use the :database keyword to specify which database we wish to query
  )