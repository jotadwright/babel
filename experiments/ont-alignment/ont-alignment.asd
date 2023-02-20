(in-package :asdf)

(defsystem :ont-alignment
  :description "Language games for ontology alignment experiment"
  :depends-on (:experiment-framework
               :utils :cl-json)
  :serial t 
  :components 
  ((:file "package")
   (:file "agent")
   (:file "db-connection")
   (:file "interaction")
   (:file "learning-sql")
))
