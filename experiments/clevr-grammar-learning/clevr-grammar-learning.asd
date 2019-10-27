(defsystem clevr-grammar-learning
  :author "Paul Van Eecke & Katrien Beuls"
  :maintainer "Paul Van Eecke & Katrien Beuls <ehai@ai.vub.ac.be>"
  :license "to be decided on"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/grammar-learning"
  :depends-on ("grammar-learning" "clevr-world")
  :serial t
  :components ((:file "package"))
  :description "Learning the CLEVR grammar.")