;;;; clevr-grammar.asd

(in-package :asdf)

(defsystem #:coco-grammar
  :description "FCG grammar for the 'cats and dogs demo'"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :monitors
               :web-interface
               :irl
               :fcg
               :clevr-world
               :clevr-primitives
               :cl-json
               :snooze
               ;:seq2seq-heuristics
               )
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
               (:file "preprocess-utterance")
               (:file "rpn-meaning")
               (:file "web-service")
               ))
