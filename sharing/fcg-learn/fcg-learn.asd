(in-package :asdf)

(defsystem :fcg-learn
  :description "A Babel package for learning construction grammars through anti-unification."
  :depends-on (:amr :fcg :irl :predicate-networks)
  :serial t
  :components ((:file "classes")
               (:file "monitors")
               (:file "routine-processing")
               (:file "meta-processing")
               (:file "pattern-finding")))
               
