(in-package :asdf)

(defsystem :mwm
  :description "Multidimensional Word Meaning experiment"
  :author "EHAI"
  :maintainer "Jens Nevens"
  :license "GPL 3.0"
  :depends-on (:test-framework
               :utils
               :web-interface
               :monitors
               :plot-raw-data
               :experiment-framework
               :meta-layer-learning
               :irl
               :fcg
               :clevr-world
               :cl-mop
               :cl-json)
  :serial t
  :components ((:file "package")
               (:file "color-conversions")
               (:file "utils")
               (:file "fuzzy-operations")
               (:file "world")
               (:file "ontology")
               (:file "lexicon")
               (:file "agent")
               (:file "alignment")
               (:file "experiment")
               (:file "interaction")
               (:file "html")
               (:file "web-monitor")
               (:file "monitors")))