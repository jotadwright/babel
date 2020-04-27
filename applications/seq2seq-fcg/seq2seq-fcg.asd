(in-package :asdf)

(defsystem #:seq2seq-fcg
  :description "Using Seq2Seq models for the FCG search heuristics"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :monitors
               :fcg
<<<<<<< HEAD
               :cl-json
               :snooze
               :clevr-grammar
               :clevr-dialog-grammar
               :clevr-evaluation
               :corpus-processing)
=======
               :clevr-grammar)
>>>>>>> origin/master
  :serial t
  :components ((:file "package")
               (:file "apply-predictions")))
