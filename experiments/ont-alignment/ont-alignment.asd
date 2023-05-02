(in-package :asdf)

(defsystem :ont-alignment
  :description "Language games for ontology alignment experiment"
  :depends-on (:experiment-framework
               :utils :cl-json :postmodern :fcg 
               :cl-postgres
               :postmodern :web-interface)
  :serial t 
  :components 
  ((:file "package")
   (:file "agent")
   (:file "interaction")
   (:file "db-connection")
   (:file "query-making")
   (:file "data-manipulation")	
))
