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
               (:file "derender")
               (:file "generate-dataset-files")
               ))