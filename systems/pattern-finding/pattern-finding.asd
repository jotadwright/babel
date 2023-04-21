(defsystem pattern-finding
  :author "Paul Van Eecke, Katrien Beuls, Jonas Doumen, Veronica Juliana Schmalz, and Jens Nevens"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "to be decided on"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/ehai-babel/"
  :depends-on (:utils
               :monitors
               :plot-raw-data
               :web-interface
               :fcg
               :irl
               :amr
               :meta-layer-learning
               :experiment-framework
               :test-framework
               :plot-raw-data
               :cl-json)
  :serial t
  :components ((:file "package")
               )
  :description "A Common Lisp package for learning construction grammars.")


