(defsystem grammar-learning
  :author "Paul Van Eecke & Katrien Beuls"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "to be decided on"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/grammar-learning"
  :depends-on ("utils" "fcg" "irl")
  :serial t
  :components ((:file "package")
               (:file "fcg-utils")
               (:file "utils")
               (:file "strategies"))
  :description "A Common Lisp package for learning construction grammars.")