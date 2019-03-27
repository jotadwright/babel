;;;; clevr-grammar.asd

(in-package :asdf)

(defsystem #:clevr-grammar
  :description "FCG grammar for CLEVR dataset"
  :author "Jens Nevens <jens@ai.vub.ac.be>"
  :license "GPL 3.0"
  :depends-on (:utils
               :monitors
               :fcg
               :cl-json)
  :serial t
  :components ((:file "package")
               (:file "fcg-utils")
               (:file "grammar")
               (:file "lex-and-morph")
               (:file "config")
               (:file "nominal")
               (:file "query")
               (:file "relate")
               (:file "count")
               (:file "exist")
               (:file "multi-hop")
               (:file "single-and")
               (:file "single-or")
               (:file "compare-integer")
               (:file "same-relate")
               (:file "comparison")))
