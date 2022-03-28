(in-package :asdf)

(defsystem :spatial-concepts
  :description "spatial concept learning experiment"
  :author "EHAI"
  :maintainer "Jens Nevens"
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
               (:file "concept")
               (:file "agent")
               (:file "alignment")
               (:file "experiment")
               (:file "interaction")
               (:file "html")
               (:file "web-monitor")
               (:file "monitors")
               (:file "misc")))