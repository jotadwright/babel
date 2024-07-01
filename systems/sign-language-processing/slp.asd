(in-package :asdf)

(defsystem :slp
  :description "Package for everything related to processing sign language forms"
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
               :xmls)
  :serial t
  :components ((:file "package")
               (:file "visualize")
               (:file "goal-tests")
               (:file "derender")
               (:file "render")))