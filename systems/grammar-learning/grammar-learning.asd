(defsystem grammar-learning
  :author "Paul Van Eecke & Katrien Beuls"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "to be decided on"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/grammar-learning"
  :depends-on ("utils" "fcg" "irl" "meta-layer-learning" "category-hierarchies")
  :serial t
  :components ((:file "package")
               (:file "fcg-utils")
               (:file "utils")
               (:file "goal-tests")
               (:file "problems-diagnostics")
               (:file "repair-add-lexical-cxn")
               (:file "repair-add-item-based-cxn")
               (:file "repair-add-holophrase-cxn"))
  :description "A Common Lisp package for learning construction grammars.")