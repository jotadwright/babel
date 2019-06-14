(in-package :asdf)

(defsystem #:clevr-world
  :description "A system that provides an interface to the CLEVR dataset"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :web-interface
               :irl
               :fcg
               :cl-json)
  :serial t
  :components ((:file "package")
               (:file "clevr-symbols")
               (:file "clevr-world")
               (:file "clevr-ontology")
               (:file "html")))