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
               :slp)
  :serial t
  :components ((:file "package")
               (:file "create-dataset")
               (:file "learn")))