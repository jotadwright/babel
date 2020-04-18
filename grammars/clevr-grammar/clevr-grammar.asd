;;;; clevr-grammar.asd

(in-package :asdf)

(defsystem #:clevr-grammar
  :description "FCG grammar for CLEVR dataset"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :monitors
               :irl
               :fcg
               :clevr-world
               :cl-json
               :seq2seq-heuristics)
  :serial t
  :components ((:file "package")
               (:file "fcg-utils")
               (:file "lex-and-morph")
               (:file "grammar")
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
               (:file "comparison")
               (:file "preprocess-utterance")))
