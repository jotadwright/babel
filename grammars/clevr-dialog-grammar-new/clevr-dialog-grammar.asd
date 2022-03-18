(in-package :asdf)

(defsystem #:clevr-dialog-grammar
  :description "FCG grammar for CLEVR Dialog dataset"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Lara Verheyen <lara.verheyen@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :monitors
               :web-interface
               :irl
               :fcg
               ;:clevr-world
               :cl-json
               :seq2seq-heuristics)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "dialog-grammar")
               (:file "adjectives")
               (:file "captions")
               (:file "clevr-dialog-symbols")
               (:file "count-questions")
               (:file "exist-questions")
               (:file "nouns")
               (:file "relations")
               (:file "seek-questions")))