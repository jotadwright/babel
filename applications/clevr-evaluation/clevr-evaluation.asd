;;;; clevr-evaluation.asd

(in-package :asdf)

(defsystem #:clevr-evaluation
  :description "Evaluating the CLEVR grammar and primitives"
  :author "Jens Nevens <jens@ai.vub.ac.be>"
  :license "GPL 3.0"
  :depends-on (:utils
               :web-interface
               :monitors
               :irl
               :fcg
               :clevr
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
               (:file "web-monitors")))