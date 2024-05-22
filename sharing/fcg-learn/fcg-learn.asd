(in-package :asdf)

(defsystem :fcg-learn
  :description "A Babel package for learning construction grammars through anti-unification."
  :depends-on (:amr :fcg :irl)
  :serial t
  :components ((:file "anti-unify-observation-cxn")
               (:file "anti-unify-cxns")))
               
