(in-package :asdf)

(defsystem #:mwm-evaluation
  :description "Integrating the multi-dimensional concepts in vqa"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :cl-json
               :cl-store
               :plot-raw-data
               :monitors
               :experiment-framework
               :web-interface
               :irl
               :fcg
               :clevr-world
               :clevr-grammar
               :mwm)
  :serial t
  :components ((:file "package")
               (:file "mwm-ontology")
               (:file "mwm-utils")
               (:file "evaluation")
               (:file "monitors")
               (:file "irl-node-test")
               (:module "primitives"
                :serial t
                :components ((:file "primitive-inventory")
                             (:file "count")
                             (:file "equal-less-greater")
                             (:file "equal")
                             (:file "exist")
                             (:file "filter")
                             (:file "intersect")
                             (:file "query")
                             (:file "relate")
                             (:file "same")
                             (:file "segment-scene")
                             (:file "union")
                             (:file "unique")))))