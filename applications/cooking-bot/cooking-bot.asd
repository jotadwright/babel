(in-package :asdf)

(defsystem :cooking-bot
  :author ""
  :maintainer ""
  :license ""
  :homepage ""
  :depends-on (:utils :test-framework :web-interface)
  :description "A cooking bot."
  :serial t
  :components ((:file "package")
               (:file "utils")

               (:module procedural-semantics
                :serial t
                :components ((:file "data-structures")
                             (:file "evaluation")))
               
               (:file "ontology")
               (:file "primitives")
               ))
