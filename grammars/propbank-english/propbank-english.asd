(in-package :asdf)

(defsystem :propbank-english
  :description "A large propbank-based construction grammar for English."
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "EHAI"
  :license "To be determined."
  :depends-on (:utils
               :monitors
               :fcg
               :nlp-tools
               :irl
               :web-interface
               :cl-propbank
               :xmls
               :cl-store)
  :serial t
  :components ((:file "package")
               (:file "de-render")
               (:file "grammar")
               (:file "visualisation")
               
               (:module learning
                :serial t
                :components ((:file "propbank-annotations")
                             (:file "evaluation")
                             (:file "learn-propbank-constructions")))
               (:file "utils")))
