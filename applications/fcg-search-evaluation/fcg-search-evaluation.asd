(in-package :asdf)

(defsystem #:fcg-search-evaluation
  :description "Evaluating the search in FCG"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :monitors
               :experiment-framework
               :plot-raw-data
               :fcg
               :clevr-world
               :clevr-grammar
               :trivial-timeout
               :seq2seq-heuristics)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "experiment")
               (:file "monitors")
               ))
