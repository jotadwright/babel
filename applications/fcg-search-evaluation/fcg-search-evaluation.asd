(in-package :asdf)

(defsystem #:fcg-search-evaluation
  :description "Evaluating the search in FCG"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :monitors
               :fcg
               :clevr-world
               :clevr-grammar
               :trivial-timeout
               :seq2seq-heuristics
               :usocket
               :cl-csv)
  :serial t
  :components ((:file "package")))
