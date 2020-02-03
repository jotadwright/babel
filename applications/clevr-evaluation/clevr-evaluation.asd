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
               :trivial-timeout)
  :serial t
  :components ((:file "package")
               (:file "irl-program-utils")
               (:file "utils")         
               (:file "functional-programs")
               (:file "preprocess-program")
               (:file "preprocess-sentence")
               (:file "evaluation")
               (:file "evaluation-parallel")
               (:file "web-monitors")
               (:module fcg-search-experiment
                :serial t
                :components ((:file "utils")
                             (:file "experiment")
                             (:file "monitors")))))