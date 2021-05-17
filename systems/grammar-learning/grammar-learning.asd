(defsystem grammar-learning
  :author "Paul Van Eecke & Katrien Beuls"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "to be decided on"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/grammar-learning"
  :depends-on ("utils" "monitors" "plot-raw-data" "web-interface" "fcg" "irl" "meta-layer-learning" "category-hierarchies" "cl-change-case")
  :serial t
  :components ((:file "package")
               (:file "fcg-utils")
               (:file "utils")
               (:file "goal-tests")
               (:file "problems-diagnostics")
               (:file "repair-add-th-links")
               (:file "repair-nothing-to-holophrase-cxn")
               (:file "repair-holophrase-to-item-based+lexical+lexical--substitution")
               (:file "repair-holophrase-to-item-based+lexical--addition")
               (:file "repair-holophrase-to-item-based+lexical+holophrase--deletion")
               (:file "repair-lexical-to-item-based-cxn")
               (:file "repair-item-based-to-lexical")
               (:file "monitors")
               (:file "web-monitors"))
  :description "A Common Lisp package for learning construction grammars.")