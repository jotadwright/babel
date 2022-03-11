(defsystem grammar-learning
  :author "Paul Van Eecke, Katrien Beuls, Jonas Doumen and Veronica Juliana Schmalz"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "to be decided on"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/ehai-babel/"
  :depends-on ("utils" "monitors" "plot-raw-data" "web-interface" "fcg" "irl" "amr" "meta-layer-learning" "cl-change-case" "experiment-framework" "test-framework" "plot-raw-data" "clevr-world" "cl-json" )
  :serial t
  :components ((:file "package")
               (:module diagnostics-and-repairs
                :serial t
                :components ((:file "fcg-utils")
                             (:file "utils")
                             (:file "goal-tests")
                             (:file "handle-fix")
                             (:file "problems-diagnostics")
                             (:file "repair-add-categorial-links")
                             (:file "repair-nothing-to-holophrase-cxn")
                             (:file "repair-holophrase-to-item-based+holistic+holistic--substitution")
                             (:file "repair-holophrase-to-item-based+holistic--addition")
                             (:file "repair-holophrase-to-item-based+holistic+holophrase--deletion")
                             (:file "repair-holistic-to-item-based-cxn")
                             (:file "repair-item-based-to-holistic")))
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
                             (:file "test-utils")
                             (:file "test-substitution-repair")
                             (:file "test-addition-repair")
                             (:file "test-deletion-repair")
                             (:file "test-holistic-to-item-based-repair"))))
                             

  :description "A Common Lisp package for learning construction grammars.")


