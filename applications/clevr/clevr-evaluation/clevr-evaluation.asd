;;;; clevr-evaluation.asd

(in-package :asdf)

(defsystem #:clevr-evaluation
  :description "Evaluating the CLEVR grammar and primitives"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :corpus-processing
               :plot-raw-data
               :monitors
               :experiment-framework
               :web-interface
               :irl
               :fcg
               :clevr-world
               :clevr-primitives
               :clevr-grammar
               :cl-json
               :cl-mop
               :trivial-timeout
               :cl-csv)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "coverage")
               (:file "accuracy")))