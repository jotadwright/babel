(in-package :asdf)

(defsystem :propbank-english
  :description "A large propbank-based construction grammar for English."
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "EHAI"
  :license "To be determined."
  :depends-on (:utils
               :monitors
               :irl
               :fcg
               :nlp-tools
               :web-interface
               :cl-propbank
               :xmls
               :cl-store
               :snooze
               :cl-json
               :trivial-timeout)
  :serial t
  :components ((:file "package")
               (:file "propbank-annotations")
               (:module utils
                :serial t
                :components ((:file "visualisation")
                             (:file "utils")
                             (:file "de-render")
                             (:file "cxn-supplier")))
               
               ;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               ;; TO DO; the following files need to be updated
               ;; to the categorial network that is integrated in FCG
               ;; instead of using the separate type-hierarchies package
               (:module learning
                :serial t
                :components ((:file "evaluation")
                             (:file "learn-propbank-constructions")))
               ;(:file "categorial-network-metrics")
               ;(:file "grammar-analysis")
               ;;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               
               ))
