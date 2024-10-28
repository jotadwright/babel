(in-package :asdf)

(defsystem :geoquery-lsfb
  :description "Package for learning grammars from the geoquery-LSFB dataset"
  :maintainer "Liesbet De Vos"
  :depends-on (:test-framework
               :utils
               :web-interface
               :monitors
               :plot-raw-data
               :experiment-framework
               :meta-layer-learning
               :irl
               :fcg
               :cl-json
               :xmls
               :au-benchmark)
  :serial t
  :components ((:file "package")
               (:module utils
                :serial t
                :components ((:file "string-manipulation")
                             (:file "xml-utils")
                             (:file "json-utils")
                             (:file "hamnosys")
                             (:file "elan-utils")
                             (:file "dataset-generation-utils")))
               (:module elan-to-predicates
                :serial t
                :components ((:file "create-elan-intervals")
                             (:file "create-predicates")
                             (:file "elan-to-predicates")))
               (:file "derender") 
               ))