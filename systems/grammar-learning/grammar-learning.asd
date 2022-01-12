(defsystem grammar-learning
  :author "Paul Van Eecke, Katrien Beuls, Jonas Doumen and Veronica Juliana Schmalz"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "to be decided on"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/ehai-babel/"
  :depends-on ("utils" "monitors" "plot-raw-data" "web-interface" "fcg" "irl" "amr" "meta-layer-learning" "cl-change-case")
  :serial t
  :components ((:file "package")
               (:module diagnostics-and-repairs
                :serial t
                :components ((:file "fcg-utils")
                             (:file "utils")
                             (:file "goal-tests")
                             (:file "problems-diagnostics")
                             (:file "repair-add-categorial-links")
                             (:file "repair-nothing-to-holophrase-cxn")
                             (:file "repair-holophrase-to-item-based+lexical+lexical--substitution")
                             (:file "repair-holophrase-to-item-based+lexical--addition")
                             (:file "repair-holophrase-to-item-based+lexical+holophrase--deletion")
                             (:file "repair-lexical-to-item-based-cxn")
                             (:file "repair-item-based-to-lexical")
                             (:file "monitors")
                             (:file "web-monitors")))
               )
  :description "A Common Lisp package for learning construction grammars.")