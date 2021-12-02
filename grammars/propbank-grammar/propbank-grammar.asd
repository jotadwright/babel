(in-package :asdf)

(defsystem :propbank-grammar
  :description "A Babel package for learning large-scale PropBank-based construction grammars."
  :author "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "To be determined."
  :depends-on (:utils :nlp-tools :cl-store :fcg :irl)
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
                             (:file "frame-visualisation")
                             (:file "extract-frames")
                             (:file "comprehend")))
               (:module learning
                :serial t
                :components ((:file "learn-propbank-constructions")))))
