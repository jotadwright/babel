(in-package :asdf)

(defsystem :ont-alignment
  :description "Language games for ontology alignment experiment"
  :depends-on (:experiment-framework
               :utils :cl-json :postmodern :queues :queues.simple-queue :fcg 
               :cl-postgres
               :postmodern)
  :serial t 
  :components 
  ((:file "package")
   (:file "agent")
   (:file "interaction")
   (:file "db-connection")
   (:file "query-making")
   (:file "data-manipulation")	
))
