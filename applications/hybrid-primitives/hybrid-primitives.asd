(in-package :asdf)

(defsystem #:hybrid-primitives
  :description "The IRL primitives for CLEVR using modular neural networks"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :web-interface
               :irl
               :fcg
               ;:cl-json
               :jonathan
               :drakma
               :clevr-world
               :parse-float)
  :serial t
  :components ((:file "package")
               (:file "server")
               (:file "get-context")
               (:file "filter")
               (:file "query")
               (:file "same")
               (:file "equal")
               (:file "count")
               (:file "exist")
               (:file "unique")
               (:file "relate")
               (:file "intersect")
               (:file "union")
               (:file "equal-less-greater")))
