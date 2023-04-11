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
               :cl-postgres
               :postmodern
               :cl-csv)
  :serial t
  :components ((:file "package")
                (:module "objects"
                 :serial t
                 :components ((:file "attribute-obj")
                              (:file "query-composer-obj")
                              (:file "node-obj")
                              (:file "tree-obj")
                              (:file "table-obj")
                              (:file "reference-info-obj")))
                (:module "db"
                 :serial t
                 :components ((:file "setup-database")
                              (:file "schema")))
                (:module "agents"
                 :serial t
                 :components ((:file "master-agent")))
                (:file "html")
                (:file "run")
                (:file "utils")))