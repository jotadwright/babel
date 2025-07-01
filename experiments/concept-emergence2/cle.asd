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
               :experiment-framework
               :irl
               :cl-store
               :com.inuoe.jzon
               :distributions
               :concept-representations
               :social-network
               :partner-selection
               )
  :serial t
  :components ((:file "package")
               (:module "world"
                :serial t
                :components ((:file "world")
                             (:file "scene")
                             (:file "topic")
                             ))
               (:module "agent"
                :serial t
                :components ((:file "usage-tracker")
                             (:file "agent")
                             (:file "adoption")
                             (:file "alignment")
                             (:file "invention")
                             (:file "conceptualisation")
                             (:file "interpretation")
                             (:file "parsing")
                             (:file "production")
                             (:file "perception")
                             ))
               (:module "construction"
                :serial t
                :components ((:file "construction")
                             (:file "lexicon")
                             (:file "update")))
               (:module "interaction"
                :serial t
                :components ((:file "experiment")
                             (:file "determine-agents")
                             (:file "before")
                             (:file "during")
                             (:file "after")
                             (:file "run")
                             (:file "switch")))
               (:module "utils"
                :serial t
                :components ((:file "utils")
                             (:file "monitors")))
               (:module "web"
                :serial t
                :components ((:file "html")
                             (:file "s-dot")
                             (:file "s-dot-diff")
                             (:file "web-monitor")))))
