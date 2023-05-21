(in-package :asdf)

;; clean

(defsystem :cle
  :description "Emergent concept learning - v2"
  :author "EHAI"
  :maintainer "Jerome Botoko Ekila <jerome@ai.vub.ac.be>"
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
               :clevr-world
               :cl-mop
               :cl-json)
  :serial t
  :components ((:file "package")
               (:module "world"
                :serial t
                :components ((:file "clevr")
                             (:file "import")
                             (:file "scene")
                             (:file "topic")
                             ))
               (:module "agent"
                :serial t
                :components ((:file "agent")
                             (:file "adoption")
                             (:file "alignment")
                             (:file "invention")
                             (:file "conceptualisation")
                             (:file "interpretation")
                             (:file "parsing")
                             (:file "production")
                             ))
               (:module "prototype"
                :serial t
                :components ((:file "prototype")
                             (:file "update")))
               (:module "concept"
                :serial t
                :components ((:file "concept")
                             (:file "shift")
                             (:file "similarity")))
               (:module "construction"
                :serial t
                :components ((:file "construction")
                             (:file "competitors")
                             (:file "update")))
               (:module "distributions"
                :serial t
                :components ((:file "gaussian")
                             (:file "divergence")
                             ))
               (:module "interaction"
                :serial t
                :components ((:file "experiment")
                             (:file "determine-agents")
                             (:file "before")
                             (:file "during")
                             (:file "after")
                             (:file "run")))))
