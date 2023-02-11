(in-package :asdf)

;; prefixed

(defsystem :qc
  :description "Internship project about query composition"
  :author "Unamur"
  :maintainer "Valentin Grilli <valentin.grilli@student.unamur.be>"
  :license "GPL 3.0"
  :depends-on (:test-framework
               :utils
               :web-interface
               :monitors
               :plot-raw-data
               :experiment-framework
               :test-framework
               :meta-layer-learning
               :irl
               :fcg 
               :cl-mop
               :cl-json
               )
  :serial t
  :components ((:file "package")
                (:file "first-step")
                (:file "tree-query")
                (:file "utils")))
