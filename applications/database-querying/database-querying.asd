(in-package :asdf)

(defsystem :database-querying
  :description "A system for querying databases using FCG"
  :depends-on (:utils :monitors :fcg
               #+:hunchentoot-available-on-this-platform :web-interface)
  :components ((:file "package")
               (:file "grammar")
               (:file "sql-interface")
               (:file "construct-sql-query")
               (:file "monitor")
               (:file "querying")))

