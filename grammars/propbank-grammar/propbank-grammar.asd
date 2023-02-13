(in-package :asdf)

(defsystem :propbank-grammar
  :description "A Babel package for learning large-scale PropBank-based construction grammars."
  :author "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "To be determined."
  :depends-on (:utils :nlp-tools :cl-store :fcg :irl :fcg-server :monitors :dexador)
  :serial t
  :components ((:file "package")
               
               (:module propbank-annotations
                :serial t
                :components ((:file "propbank-annotations")))
               (:module fcg-components
                :serial t
                :components ((:file "de-render")
                             (:file "goal-tests")
                             (:file "node-tests")
                             (:file "cxn-supplier")
                             (:file "hash-mode")
                             (:file "heuristics")
                             (:file "frame-visualisation")
                             (:file "extract-frames")
                             (:file "comprehend")))
               (:module learning
                :serial t
                :components ((:file "cxn-name")
                             (:file "cxn-schema")
                             (:file "utils")
                             (:file "learn-propbank-constructions")))
               (:module grammar-metrics
                :serial t
                :components ((:file "categorial-network-metrics")
                             (:file "grammar-analysis")))
               (:module cleaning-and-evaluation
                :serial t
                :components ((:file "clean")
                             (:file "evaluation")
                             (:file "argm-corpus")))
               (:module monitors
                :serial t
                :components ((:file "categorial-network")))
               (:module web-service
                :serial t
                :components ((:file "fcg-server-route")))))
