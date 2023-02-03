(in-package :asdf)

(defsystem :ont-alignment
  :description "Language games for ontology alignment experiment"
  :author "EHAI"
  :maintainer "Alexane Jouglar"
  :depends-on (:utils
               :plot-raw-data
               :monitors
               :experiment-framework
               :test-framework
               :irl
               :fcg)
  :serial t
  :components ((:file "package")
               (:file "db-connection")))
