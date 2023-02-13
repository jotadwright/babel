;;;; clevr-grammar.asd

(in-package :asdf)

(defsystem #:clevr-grammar-v1
  :description "FCG grammar for CLEVR dataset"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :monitors
               :irl
               :fcg
               :clevr-world
               :clevr-primitives
               :cl-json
               :seq2seq-heuristics)
  :serial t
  :components ((:file "package")
               (:module "fcg-utils"
                :serial t
                :components ((:file "cxn-supplier")
                             (:file "de-render")
                             (:file "node-test")))
               (:file "lex-and-morph")
               (:file "rpn-meaning")
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
