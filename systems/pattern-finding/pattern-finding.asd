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
               :clevr-world
               :cl-json)
  :serial t
  :components ((:file "package")
               (:module utils
                :serial t
                :components ((:file "fcg-utils")
                             (:file "utils")
                             (:file "diff")))
               (:module diagnostics-and-repairs
                :serial t
                :components ((:file "goal-tests")
                             (:file "handle-fix")
                             (:file "problems-diagnostics")
                             (:file "repair-add-categorial-links")
                             (:file "repair-nothing-to-holophrase-cxn")
                             (:file "repair-holistic-substitution")
                             (:file "repair-holistic-addition")
                             (:file "repair-holistic-deletion")
                             (:file "repair-holistic-to-item-based-cxn")
                             (:file "repair-item-based-to-holistic")
                             (:file "repair-item-based-substitution")))
               (:module experiment-setup
                :serial t
                :components ( (:file "run-helpers")
                              (:file "grammar")
                              (:file "agent")
                              (:file "learner")
                              (:file "alignment")
                              (:file "experiment")
                              (:file "interaction")
                              (:module monitors
                               :serial t
                               :components ( (:file "web-monitors")
                                             (:file "export-monitors")
                                             (:file "lisp-monitors")
                                             (:file "csv-monitors")
                                             ))))
               (:module tests
                :serial t
                :components ((:file "utils")
                             ;(:file "test-utils")
                             (:module irl
                              :serial t
                              :components ((:file "test-substitution-repair-irl")
                                           (:file "test-addition-repair-irl")
                                           (:file "test-deletion-repair-irl")
                                           (:file "test-add-categorial-links-repair-irl"))))))
  :description "A Common Lisp package for learning construction grammars.")


