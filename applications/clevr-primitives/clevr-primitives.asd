(in-package :asdf)

(defsystem #:clevr-primitives
  :description "The IRL primitives for CLEVR"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :web-interface
               :irl
               :fcg
               :cl-json
               :clevr)
  :serial t
  :components ((:file "package")
               ;(:file "world")
               ;(:file "html")
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
