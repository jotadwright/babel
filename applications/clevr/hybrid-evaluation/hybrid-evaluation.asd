(in-package :asdf)

(defsystem #:hybrid-evaluation
  :description "Evaluating the Hybrid IRL primitives on the CLEVR dataset"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :monitors
               :web-interface
               :irl
               :fcg
               :jonathan
               :clevr-world
               :clevr-grammar
               :hybrid-primitives)
  :serial t
  :components ((:file "package")
               ))
