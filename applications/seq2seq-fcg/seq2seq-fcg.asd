(in-package :asdf)

(defsystem #:seq2seq-fcg
  :description "Using Seq2Seq models for the FCG search heuristics"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :monitors
               :fcg
               :cl-json
               :snooze
               :clevr-grammar
               :clevr-evaluation)
  :serial t
  :components ((:file "package")
               (:file "server")
               (:file "apply-predictions")))
